use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;

#[derive(Debug)]
pub enum Task {
    BuildProject(PathBuf),
    BuildModule(PathBuf),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct TaskId(usize);

#[derive(Debug)]
pub(crate) struct SubmittedTask {
    pub task: Task,
    pub id: TaskId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TaskState {
    Submitted,
    Waiting,
    Finished,
}

#[derive(Debug)]
struct TaskInfo {
    state: TaskState,
    depends: Vec<TaskId>,
}

impl TaskInfo {
    fn new() -> TaskInfo {
        TaskInfo {
            state: TaskState::Submitted,
            depends: Vec::new(),
        }
    }

    fn set_state(mut self, state: TaskState) -> TaskInfo {
        self.state = state;
        self
    }
}

pub(crate) enum Resolution {
    Dispatched,
    Deferred,
}

#[derive(Debug)]
pub(crate) struct TaskGraph {
    info: HashMap<TaskId, TaskInfo>,
    queue: VecDeque<SubmittedTask>,
    next_id: TaskId,
}

impl TaskGraph {
    pub(crate) fn new() -> TaskGraph {
        TaskGraph {
            info: HashMap::default(),
            queue: VecDeque::default(),
            next_id: TaskId(0),
        }
    }

    pub(crate) fn submit(&mut self, task: Task) -> TaskId {
        let id = self.next_id;
        self.next_id.0 += 1;

        let submitted = SubmittedTask { task, id };
        self.info.insert(id, TaskInfo::new());
        self.queue.push_back(submitted);

        id
    }

    pub(crate) fn get_state(&self, id: TaskId) -> Option<TaskState> {
        match self.info.get(&id) {
            Some(info) => Some(info.state),
            None if id.0 < self.next_id.0 => Some(TaskState::Finished),
            None => None,
        }
    }

    pub(crate) fn pop(&mut self) -> Option<SubmittedTask> {
        self.queue.pop_front()
    }
}
