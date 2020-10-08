use crate::evaluator::Evaluation;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::rc::Rc;

#[derive(PartialEq, Eq, Debug)]
pub(crate) struct Scope {
    evaluations: HashMap<String, Rc<Evaluation>>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub(crate) fn new() -> Scope {
        Scope {
            evaluations: HashMap::default(),
            parent: None,
        }
    }

    pub(crate) fn from<T: IntoIterator<Item = (String, Rc<Evaluation>)>>(evaluations: T) -> Scope {
        Scope {
            evaluations: HashMap::from_iter(evaluations),
            parent: None,
        }
    }

    pub(crate) fn wrap(
        evaluations: Vec<(String, Rc<Evaluation>)>,
        parent: Rc<RefCell<Scope>>,
    ) -> Scope {
        Scope {
            evaluations: HashMap::from_iter(evaluations),
            parent: Some(parent),
        }
    }

    pub(crate) fn insert_evaluation(&mut self, identifier: String, evaluation: Rc<Evaluation>) {
        self.evaluations.insert(identifier, evaluation);
    }

    pub(crate) fn get_evaluation(&self, identifier: &str) -> Option<Rc<Evaluation>> {
        match self.evaluations.get(identifier) {
            Some(evaluation) => Some(evaluation.clone()),
            None => match self.parent {
                Some(ref parent) => parent.borrow().get_evaluation(identifier),
                None => None,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::evaluator::Evaluation;
    use std::rc::Rc;

    #[test]
    fn parent_scope() -> std::result::Result<(), String> {
        let scope_evaluations = vec![(String::from("abc"), Rc::new(Evaluation::Int(8)))];

        let scope = Scope::from(scope_evaluations);

        match scope.get_evaluation("abc") {
            Some(evaluation) => {
                assert_eq!(evaluation, Rc::new(Evaluation::Int(8)));
                Ok(())
            }
            None => Err(String::from("Could not get evaluation from scope")),
        }
    }
}
