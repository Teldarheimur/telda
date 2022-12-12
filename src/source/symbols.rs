use super::{SourceLocation, Result as SourceResult, Error, ErrorType};

pub struct Symbols {
    pub labels: Vec<Box<str>>,
    pub id_to_pos: Vec<Result<u16, Vec<SourceLocation>>>,
    pub globals: Vec<bool>,
}

impl Symbols {
    pub fn new() -> Self {
        Self { labels: Vec::new(), globals: Vec::new(), id_to_pos: Vec::new() }
    }
    fn find_id(&mut self, lbl: &str) -> usize {
        if let Some(i) = self.labels.iter().position(|l| &**l == lbl) {
            i
        } else {
            let i = self.labels.len();
            self.labels.push(lbl.to_owned().into_boxed_str());
            self.id_to_pos.push(Err(Vec::with_capacity(1)));
            i
        }
    }
    pub fn set_label(&mut self, lbl: &str, pos: u16, loc: SourceLocation) -> SourceResult<()> {
        let id = self.find_id(lbl);
        match &mut self.id_to_pos[id] {
            &mut Ok(p) => {
                return Err(Error::new(loc.source, loc.line_number, ErrorType::Other(
                    format!("Label {lbl} already had pos {p:03x} but is now being set to {pos:03x}").into_boxed_str()
                )));
            }
            e @ Err(_) => *e = Ok(pos),
        }
        Ok(())
    }
    pub fn read_label(&mut self, lbl: &str, loc: SourceLocation) -> usize {
        let id = self.find_id(lbl);
        match &mut self.id_to_pos[id] {
            Ok(_) => (),
            Err(v) => v.push(loc),
        }

        id
    }
    pub fn set_global(&mut self, id: usize) {
        if id >= self.globals.len() {
            self.globals.resize(id+1, false);
        }
        self.globals[id] = true;
    }
    // fn is_global(&self, id: usize) -> bool {
    //     self.globals.get(id).copied().unwrap_or(false)
    // }
}
