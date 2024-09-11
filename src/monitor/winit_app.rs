use std::num::NonZeroU32;
/// Common boilerplate for setting up a winit application.
use std::rc::Rc;
use std::sync::mpsc::Sender;
use std::thread::yield_now;
use std::time::{Duration, Instant};

use softbuffer::{Context, Surface};
use winit::application::ApplicationHandler;
use winit::dpi::PhysicalSize;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::window::{Window, WindowAttributes, WindowId};

use super::card::FrameBuf;
use super::{FpsRingBuffer, FACTOR, HEIGHT, WIDTH};

/// Run a Winit application.
#[allow(unused_mut)]
pub(crate) fn run_app(event_loop: EventLoop<()>, mut app: impl ApplicationHandler<()> + 'static) {
    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    event_loop.run_app(&mut app).unwrap();

    #[cfg(any(target_arch = "wasm32", target_arch = "wasm64"))]
    winit::platform::web::EventLoopExtWebSys::spawn_app(event_loop, app);
}

/// Create a window from a set of window attributes.
#[allow(dead_code)]
pub(crate) fn make_window(
    elwt: &ActiveEventLoop,
    f: impl FnOnce(WindowAttributes) -> WindowAttributes,
) -> Rc<Window> {
    let attributes = f(WindowAttributes::default());
    #[cfg(target_arch = "wasm32")]
    let attributes = winit::platform::web::WindowAttributesExtWebSys::with_append(attributes, true);
    let window = elwt.create_window(attributes);
    Rc::new(window.unwrap())
}

pub(crate) struct WinitApp {
    state: Option<(Rc<Window>, Surface<Rc<Window>, Rc<Window>>)>,

    input_cable: Sender<u8>,
    last_fpss: FpsRingBuffer,
    last_draw: Instant,
    framebuf: FrameBuf,
    size: PhysicalSize<u32>,
}

impl WinitApp {
    /// Create a new application.
    pub(crate) fn new(input_cable: Sender<u8>, framebuf: FrameBuf) -> Self {
        let now = Instant::now();
        Self {
            state: None,

            last_draw: now,
            last_fpss: FpsRingBuffer::new(),
            framebuf,
            input_cable,
            size: PhysicalSize::new(FACTOR * WIDTH, FACTOR * HEIGHT),
        }
    }
    fn init(elwt: &ActiveEventLoop) -> (Rc<Window>, Surface<Rc<Window>, Rc<Window>>) {
        let window = make_window(elwt, |w| w
            .with_title("telda")
            .with_inner_size(PhysicalSize::new(FACTOR * WIDTH, FACTOR * HEIGHT)) 
            .with_min_inner_size(PhysicalSize::new(WIDTH, HEIGHT))
        );

        let context = Context::new(window.clone()).unwrap();
        let surface = Surface::new(&context, window.clone()).unwrap();

        (window, surface)
    }
}

impl ApplicationHandler for WinitApp {
    fn resumed(&mut self, el: &ActiveEventLoop) {
        debug_assert!(self.state.is_none());
        self.state = Some(Self::init(el));
    }

    fn suspended(&mut self, _event_loop: &ActiveEventLoop) {
        let state = self.state.take();
        debug_assert!(state.is_some());
        drop(state);
    }

    fn window_event(
        &mut self,
        elwt: &ActiveEventLoop,
        window_id: WindowId,
        event: WindowEvent,
    ) {
        let (window, surface) = self.state.as_mut().unwrap();
        if window_id != window.id() {
            return;
        }

        match event {
            WindowEvent::RedrawRequested => {
                'draw: {
                    match self.framebuf.handle() {
                        Ok(false) => break 'draw,
                        Ok(true) => (),
                        Err(_) => {
                            elwt.exit();
                            break 'draw;
                        }
                    }

                    let mut buffer = surface.buffer_mut().unwrap();

                    if buffer.is_empty() { break 'draw; }

                    let scale = {
                        let w_scale = self.size.width / WIDTH;
                        let h_scale = self.size.height / HEIGHT;
                        w_scale.min(h_scale)
                    };
                    let w_drawed = scale * WIDTH;
                    let w_offset = (self.size.width - w_drawed) / 2;
                    let h_drawed = scale * HEIGHT;
                    let h_offset = (self.size.height - h_drawed) / 2;

                    for y in 0..self.size.height {
                        if y < h_offset || y >= h_offset + h_drawed {
                            buffer[y as usize * self.size.width as usize..][..self.size.width as usize].iter_mut().for_each(|p| *p = 0);
                            continue;
                        }

                        for x in 0..self.size.width {
                            let rgb_pixel = &mut buffer[y as usize * self.size.width as usize + x as usize];
                            if x < w_offset || x >= w_offset + w_drawed {
                                *rgb_pixel = 0x00_000000;
                            } else {
                                let c = self.framebuf.buf[((y - h_offset) / scale) as usize][((x - w_offset) / scale) as usize];
                                *rgb_pixel = self.framebuf.palette[c as usize];
                            }
                        }
                    }

                    if buffer
                        .present()
                        .map_err(|e| eprintln!("buffer presentation failed: {}", e))
                        .is_err()
                    {
                        elwt.exit();
                        return;
                    }
                }

                let now = Instant::now();
                let fps = 1. / (now - self.last_draw).as_secs_f32();
                self.last_fpss.add(fps);
                let avg_fps = self.last_fpss.avg();
                window.set_title(&format!("telda - FPS {avg_fps:.0}"));
                self.last_draw = now;

                elwt.set_control_flow(ControlFlow::wait_duration(Duration::from_secs_f32(1./60.)));
                yield_now();
            }
            WindowEvent::Resized(new_size) => {
                self.size = new_size;
                surface.resize(
                    NonZeroU32::new(new_size.width).unwrap(),
                    NonZeroU32::new(new_size.height).unwrap(),
                ).unwrap();
                window.request_redraw();
            }
            WindowEvent::Destroyed | WindowEvent::CloseRequested => {
                elwt.exit();
            }
            WindowEvent::KeyboardInput { device_id: _, event, is_synthetic: _ } => {
                eprintln!("{event:?}");
                if let Some(txt) = event.text {
                    for &b in txt.as_bytes() {
                        self.input_cable.send(b).unwrap();
                    }
                }
                window.request_redraw();
            }
            _ => (),
        }
    }

    fn about_to_wait(&mut self, _event_loop: &ActiveEventLoop) {
        if let Some(state) = &self.state {
            state.0.request_redraw();
        }
    }

    // fn device_event(
    //         &mut self,
    //         event_loop: &ActiveEventLoop,
    //         device_id: winit::event::DeviceId,
    //         event: winit::event::DeviceEvent,
    //     ) {
        
    // }

    fn memory_warning(&mut self, _event_loop: &ActiveEventLoop) {
        eprintln!("memory warning");
    }
}