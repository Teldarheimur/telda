use std::{fs::File, io::{BufReader, BufRead, self}, path::Path, collections::HashMap, hash::Hash};

pub const SYMBOL_FILE_EXT: &str = "tsym";
pub const NON_GLOBAL_SYMBOL_FILE_EXT: &str = "local.tsym";
pub const BINARY_EXT: &str = "tbin";
pub const SOURCE_EXT: &str = "telda";

pub fn read_symbol_file<T: SymbolCollector<String, u16>, U: SymbolCollector<u16, String>, P: AsRef<Path>>(path: P, labels: &mut T, pos_to_labels: &mut U) -> io::Result<()> {
    let f = File::open(path)?;

    for line in BufReader::new(f).lines() {
        let line = line?;
        let colon = line.find(':').unwrap();
        let lbl = line[..colon].to_owned();
        let pos = u16::from_str_radix(&line[colon+4..], 16).unwrap();
        labels.insert(
            lbl.clone(),
            pos
        );
        pos_to_labels.insert(
            pos,
            lbl
        );
    }

    Ok(())
}

pub trait SymbolCollector<K, V> {
    fn insert(&mut self, k: K, v: V);
}

impl<K: Eq + Hash, V> SymbolCollector<K, V> for HashMap<K, V> {
    #[inline(always)]
    fn insert(&mut self, k: K, v: V) {
        HashMap::insert(self, k, v);
    }
}

impl<K, V> SymbolCollector<K, V> for () {
    #[inline(always)]
    fn insert(&mut self, _k: K, _v: V) {}
}
