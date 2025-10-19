use std::collections::HashMap;
use std::ops::Deref;

use generational_arena::{Arena, Index};
use iter_tools::Itertools;

use crate::{Id, Normalized, Term, error::Spanned};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
#[repr(transparent)]
pub struct ClassId(pub Index);

pub struct Egraph {
    pub kv: HashMap<Id, ClassId>,
    pub nodes: Arena<Spanned<Term<Normalized>>>,
    pub classes: Arena<Class>,
}

pub struct Class {
    pub nodes: Vec<Index>,
}

impl Default for Egraph {
    fn default() -> Self {
        Self::new()
    }
}

impl Egraph {
    pub fn new() -> Self {
        Self {
            kv: HashMap::new(),
            nodes: Arena::new(),
            classes: Arena::new(),
        }
    }

    pub fn insert(&mut self, term: Spanned<Term<Normalized>>) -> Index {
        let index = self.nodes.insert(term);
        self.classes.insert(Class { nodes: vec![index] });
        index
    }

    pub fn store(&mut self, term: Spanned<Term<Normalized>>) -> Id {
        let id = crate::id();
        let index = self.nodes.insert(term);
        let class_index = self.classes.insert(Class { nodes: vec![index] });
        self.kv.insert(id, ClassId(class_index));
        id
    }

    pub fn merge(&mut self) {
        let merges = self.collect_merges();
        let combined = self
            .combine_merges(merges)
            .into_iter()
            .map(|group| {
                (
                    group,
                    ClassId(self.classes.insert(Class { nodes: Vec::new() })),
                )
            })
            .collect::<Vec<_>>();
        for (_, node) in self.nodes.iter_mut() {
            for (from, to) in combined
                .iter()
                .flat_map(|(group, to)| group.iter().zip(std::iter::repeat(to)))
            {
                node.replace(*from, *to);
            }
        }
        for (group, new_clas_id) in &combined {
            for class_id in group {
                for (_, value) in self.kv.iter_mut() {
                    if *value == *class_id {
                        *value = *new_clas_id;
                    }
                }
            }
        }
        for (group, new_class_id) in combined {
            let new_nodes = group
                .into_iter()
                .flat_map(|class_id| self.classes.remove(class_id.0).unwrap().nodes)
                .collect::<Vec<_>>();
            self.classes.get_mut(new_class_id.0).unwrap().nodes = new_nodes;
        }
    }

    fn collect_merges(&mut self) -> Vec<(ClassId, ClassId)> {
        let mut merges: Vec<(ClassId, ClassId)> = Vec::new();
        for combi in self.classes.iter().permutations(2) {
            let [(id_a, class_a), (id_b, class_b)] = combi.as_slice() else {
                continue;
            };
            if id_a == id_b {
                continue;
            }
            if class_a
                .nodes
                .iter()
                .flat_map(|a| {
                    std::iter::repeat(self.nodes.get(*a).unwrap())
                        .zip(class_b.nodes.iter().map(|b| self.nodes.get(*b).unwrap()))
                })
                .any(|(a, b)| a.deref() == b.deref())
            {
                merges.push((ClassId(*id_a), ClassId(*id_b)));
            }
        }
        merges
    }

    fn combine_merges(&mut self, merges: Vec<(ClassId, ClassId)>) -> Vec<Vec<ClassId>> {
        let mut combined: Vec<Vec<ClassId>> = Vec::new();
        for (from, to) in merges {
            let from_index = combined.iter().position(|group| group.contains(&from));
            let to_index = combined.iter().position(|group| group.contains(&to));
            match (from_index, to_index) {
                (Some(fi), Some(ti)) if fi != ti => {
                    if fi < ti {
                        let to_group = combined.remove(ti);
                        combined[fi - 1].extend(to_group);
                    } else {
                        let from_group = combined.remove(fi);
                        combined[ti - 1].extend(from_group);
                    }
                }
                (Some(_), Some(_)) => {
                    // already in the same group
                }
                (Some(fi), None) => {
                    combined[fi].push(to);
                }
                (None, Some(ti)) => {
                    combined[ti].push(from);
                }
                (None, None) => {
                    combined.push(vec![from, to]);
                }
            }
        }
        combined
    }
}

pub trait Replace {
    fn replace(&mut self, from: ClassId, to: ClassId);
}

impl Replace for Term<Normalized> {
    fn replace(&mut self, from: ClassId, to: ClassId) {
        todo!()
    }
}
