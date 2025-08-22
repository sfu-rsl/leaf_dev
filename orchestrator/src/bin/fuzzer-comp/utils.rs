macro_rules! no_order_wrapper {
    ($v:vis $name:ident($type:ty)) => {
        #[derive(derive_more::From, derive_more::Into)]
        $v struct $name(pub $type);

        impl PartialEq for $name {
            fn eq(&self, _: &Self) -> bool {
                true
            }
        }

        impl Eq for $name {}

        impl PartialOrd for $name {
            fn partial_cmp(&self, _: &Self) -> Option<core::cmp::Ordering> {
                Some(core::cmp::Ordering::Equal)
            }
        }

        impl Ord for $name {
            fn cmp(&self, _: &Self) -> core::cmp::Ordering {
                core::cmp::Ordering::Equal
            }
        }
    };
}
pub(crate) use no_order_wrapper;

pub(crate) mod priority_channel {
    use std::sync::Arc;

    use futures::Stream;
    use tokio::sync::{Mutex, mpsc};

    mod shared {
        use std::collections::BinaryHeap;

        use super::*;

        pub(super) struct PermitToken;

        impl PermitToken {
            fn new() -> Self {
                Self
            }
        }

        pub(super) type SenderImpl = mpsc::UnboundedSender<PermitToken>;
        pub(super) type ReceiverImpl = mpsc::UnboundedReceiver<PermitToken>;

        pub(super) struct Shared<T: Ord> {
            heap: Mutex<BinaryHeap<T>>,
        }

        impl<T: Ord> Default for Shared<T> {
            fn default() -> Self {
                Self {
                    heap: Mutex::new(BinaryHeap::new()),
                }
            }
        }

        impl<T: Ord> Shared<T> {
            pub(super) async fn push(&self, item: T, channel: &SenderImpl) {
                let mut guard = self.heap.lock().await;
                guard.push(item);
                channel
                    .send(PermitToken::new())
                    .expect("Receiver is dropped");
                drop(guard);
            }

            pub(super) async fn pop(&self, _token: PermitToken) -> T {
                let mut guard = self.heap.lock().await;
                guard
                    .pop()
                    .expect("Heap is expected to have an item when a token is received")
            }
        }
    }

    use shared::{ReceiverImpl, SenderImpl, Shared};

    pub(crate) struct Sender<T: Ord> {
        shared: Arc<Shared<T>>,
        tx: SenderImpl,
    }

    pub(crate) struct Receiver<T: Ord> {
        shared: Arc<Shared<T>>,
        rx: ReceiverImpl,
    }

    impl<T: Ord> Sender<T> {
        pub(crate) async fn send(&self, item: T) {
            self.shared.push(item, &self.tx).await;
        }
    }

    impl<T: Ord> Receiver<T> {
        pub(crate) async fn recv(&mut self) -> Option<T> {
            let token = self.rx.recv().await?;
            Some(self.shared.pop(token).await)
        }

        pub(crate) fn to_stream(self) -> impl Stream<Item = T> {
            futures::stream::unfold(self, |mut this| async move {
                match this.recv().await {
                    Some(item) => Some((item, this)),
                    None => None,
                }
            })
        }
    }

    pub(crate) fn channel<T: Ord>() -> (Sender<T>, Receiver<T>) {
        let shared = Arc::new(Shared::default());
        let (tx, rx) = mpsc::unbounded_channel();
        (
            Sender {
                shared: shared.clone(),
                tx,
            },
            Receiver { shared, rx },
        )
    }
}

pub(crate) type GenericError = Box<dyn core::error::Error + Send + Sync>;
