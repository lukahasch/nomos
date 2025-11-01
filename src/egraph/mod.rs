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

    pub fn insert(&mut self, term: Spanned<Term<Normalized>>) -> ClassId {
        let index = self.nodes.insert(term);
        let index = self.classes.insert(Class { nodes: vec![index] });
        ClassId(index)
    }

    pub fn store(&mut self, term: Spanned<Term<Normalized>>) -> Id {
        let id = crate::id();
        let index = self.nodes.insert(term);
        let class_index = self.classes.insert(Class { nodes: vec![index] });
        self.kv.insert(id, ClassId(class_index));
        id
    }

    pub fn log(&self) {
        log::info!(
            "Egraph contains {} nodes, and {} classes",
            self.nodes.len(),
            self.classes.len()
        );
    }

    pub fn rebuild(&mut self) {
        log::info!("Egraph rebuild");
        let merges = self.collect_merges();
        let combined = self
            .combine_merges(merges)
            .into_iter()
            .map(|mut group| {
                group.sort_unstable_by(|a, b| {
                    self.classes
                        .get(a.0)
                        .unwrap()
                        .nodes
                        .len()
                        .cmp(&self.classes.get(b.0).unwrap().nodes.len())
                });
                (group.pop().unwrap(), group)
            })
            .collect::<Vec<_>>();
        let empty = combined.is_empty();
        for (_, node) in self.nodes.iter_mut() {
            for (from, to) in combined
                .iter()
                .flat_map(|(to, group)| group.iter().zip(std::iter::repeat(to)))
            {
                node.replace(*from, *to);
            }
        }
        for (new_clas_id, group) in &combined {
            for class_id in group {
                for (_, value) in self.kv.iter_mut() {
                    if *value == *class_id {
                        *value = *new_clas_id;
                    }
                }
            }
        }
        for (new_class_id, group) in combined {
            let new_nodes = group
                .into_iter()
                .flat_map(|class_id| self.classes.remove(class_id.0).unwrap().nodes)
                .collect::<Vec<_>>();
            self.classes.get_mut(new_class_id.0).unwrap().nodes = new_nodes;
        }
        self.log();
        self.gc();
        self.log();
        if !empty {
            become self.rebuild();
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

    pub fn gc(&mut self) {
        log::info!("Egraph GC");
    }

    pub fn extract_class(
        &self,
        class_id: ClassId,
        eval: impl Evaluator,
    ) -> Option<&Spanned<Term<Normalized>>> {
        let class = self.classes.get(class_id.0)?;
        let best_node = class
            .nodes
            .iter()
            .filter_map(|node_id| self.nodes.get(*node_id))
            .map(|node| (node, eval.evaluate(self, node)))
            .min_by(|a, b| a.1.partial_cmp(&b.1).unwrap())
            .map(|(node, _)| node)?;
        Some(best_node)
    }

    pub fn extract(&self, id: Id, eval: impl Evaluator) -> Option<&Spanned<Term<Normalized>>> {
        let class_id = self.kv.get(&id)?;
        self.extract_class(*class_id, eval)
    }
}

pub trait Node {
    fn replace(&mut self, from: ClassId, to: ClassId);
}

impl Node for Term<Normalized> {
    fn replace(&mut self, from: ClassId, to: ClassId) {
        match self {
            Term::List(l) => {
                for item in l.iter_mut() {
                    if *item == from {
                        *item = to;
                    }
                }
            }
            Term::Integer(_) => {}
            _ => todo!("{}", self.as_ref()),
        }
    }
}

pub trait Evaluator {
    fn evaluate(&self, egraph: &Egraph, term: &Spanned<Term<Normalized>>) -> f64;
}

impl Evaluator for () {
    fn evaluate(&self, _egraph: &Egraph, _term: &Spanned<Term<Normalized>>) -> f64 {
        0.0
    }
}
