// A Bad but Safe Doubly-Linked Deque

use std::rc::Rc;
use std::cell::RefCell;

pub struct List<T> {
    head: List<T>,
    tail: List<T>
}

type Link<T> = Option<Rc<RefCell<Node<T>>>>;

struct Node<T> {
    elem: T,
    next: Link<T>,
    prev: Link<T>
}