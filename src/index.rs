use crate::{symbols::*, *};

impl Index {
    pub fn add(&mut self, clause: &Clause) {
        for i in 0..clause.atoms.len() {
            let index = if clause.polarity(i) {
                &mut self.positive
            } else {
                &mut self.negative
            };

            let atom = &clause.atoms[i];

            if index.len() <= atom.predicate {
                index.resize_with(atom.predicate + 1, || vec![])
            }
            index[atom.predicate].push(ClausePosition::new(clause.id, i));
        }

        if clause.is_unit() {
            self.units.push(clause.id);
        }
    }
}
