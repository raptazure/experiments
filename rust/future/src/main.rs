use async_std::task::{sleep, spawn};
use pin_project_lite::pin_project;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::Duration;

// async fn sleepus() {
//     for i in 1..=10 {
//         println!("Sleepus {}", i);
//         sleep(Duration::from_millis(500)).await;
//     }
// }

// fn sleepus() -> impl std::future::Future<Output = ()> {
//     for i in 1..=10 {
//         println!("Sleepus {}", i);
//         sleep(Duration::from_millis(500));
//     }
//     async_std::future::ready(())
// }

// fn sleepus() -> impl std::future::Future<Output = ()> {
//     async {
//         for i in 1..=10 {
//             println!("Sleepus {}", i);
//             sleep(Duration::from_millis(500)).await;
//         }
//     }
// }

struct DoNothing;

impl Future for DoNothing {
    type Output = ();
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Ready(())
    }
}

fn sleepus() -> impl Future<Output = ()> {
    for i in 1..=10 {
        println!("Sleepus {}", i);
        sleep(Duration::from_millis(500));
    }
    DoNothing
}

async fn sleepus_wrap() {
    DoNothing.await
}

async fn interruptus() {
    for i in 1..=5 {
        println!("Interruptus {}", i);
        sleep(Duration::from_millis(1000)).await;
    }
}

struct SleepPrint<Fut> {
    sleep: Fut,
}

impl<Fut: Future<Output = ()>> Future for SleepPrint<Fut> {
    type Output = ();
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // project the Pin<&mut Self> into a Pin<&mut Fut> so that we can work
        // on the underlying sleep Future.
        let sleep: Pin<&mut Fut> = unsafe { self.map_unchecked_mut(|s| &mut s.sleep) };

        match sleep.poll(cx) {
            Poll::Ready(()) => {
                println!("Inside SleepPrint");
                Poll::Ready(())
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

fn sleepus_demo() -> impl Future<Output = ()> {
    SleepPrint {
        sleep: sleep(Duration::from_millis(3000)),
    }
}

pin_project! {
    struct TwoFutures<Fut1, Fut2> {
        first_done: bool,
        #[pin]
        first: Fut1,
        #[pin]
        second: Fut2,
    }
}

impl<Fut1: Future, Fut2: Future> Future for TwoFutures<Fut1, Fut2> {
    type Output = Fut2::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();

        if !*this.first_done {
            if let Poll::Ready(_) = this.first.poll(cx) {
                *this.first_done = true;
            }
        }

        if *this.first_done {
            this.second.poll(cx)
        } else {
            Poll::Pending
        }
    }
}

fn sleepus_two_fut() -> impl Future<Output = ()> {
    TwoFutures {
        first_done: false,
        first: sleep(Duration::from_millis(3000)),
        second: async {
            println!("Hello TwoFutures");
        },
        // second: {
        //     println!("Hello TwoFutures");
        //     async_std::future::ready(())
        // },
    }
}

enum AndThen<Fut1, Fut2, F> {
    First(Fut1, F),
    Second(Fut2),
}

impl<Fut1, Fut2, F> Future for AndThen<Fut1, Fut2, F>
where
    Fut1: Future,
    Fut2: Future,
    F: Fn(Fut1::Output) -> Fut2,
{
    type Output = Fut2::Output;

    fn poll(self: Pin<&mut Self>, ctx: &mut Context) -> Poll<Self::Output> {
        let state = unsafe { self.get_unchecked_mut() };
        loop {
            match state {
                AndThen::First(fut1, f) => match unsafe { Pin::new_unchecked(fut1) }.poll(ctx) {
                    Poll::Pending => {
                        return Poll::Pending;
                    }
                    Poll::Ready(x) => {
                        *state = AndThen::Second(f(x));
                    }
                },
                AndThen::Second(fut2) => {
                    return unsafe { Pin::new_unchecked(fut2) }.poll(ctx);
                }
            }
        }
    }
}

trait FutureAndThen: Future {
    fn and_then<F: Fn(Self::Output) -> Fut2, Fut2: Future>(self, f: F) -> AndThen<Self, Fut2, F>
    where
        Self: Sized,
    {
        AndThen::First(self, f)
    }
}

impl<Fut: Future> FutureAndThen for Fut {}

// like monad >>= (?)
fn sleepus_andthen() -> impl Future<Output = ()> {
    println!("Sleepus 1");
    sleep(Duration::from_millis(500)).and_then(|()| {
        println!("Sleepus 2");
        sleep(Duration::from_millis(500)).and_then(|()| {
            println!("Sleepus 3");
            sleep(Duration::from_millis(500)).and_then(|()| {
                println!("Sleepus 4");
                async_std::future::ready(())
            })
        })
    })
}

// `.await` is doing exactly what `and_then` is doing
// rewrite `sleepus` to use `.await` instead of `and_then`
async fn sleepus_await() {
    println!("Sleepus 1");
    sleep(Duration::from_millis(500)).await;
    println!("Sleepus 2");
    sleep(Duration::from_millis(500)).await;
    println!("Sleepus 3");
    sleep(Duration::from_millis(500)).await;
    println!("Sleepus 4");
}

// a single operating system thread making non-blocking calls
// the attribute below automatically wraps `main` with async-std's executor
#[async_std::main]
async fn main() {
    let sleepus = spawn(sleepus());
    interruptus().await;

    sleepus.await;
}

// Each executor is capable of managing multiple tasks.
// Each task is working on producing the output of a single Future.
// And like with threads, spawn additional tasks to get concurrent running.
// fn main() {
//     async_std::task::block_on(async {
//         let sleepus = spawn(sleepus());
//         interruptus().await;

//         sleepus.await;
//     })
// }

// Futures and async/.await implement a form of cooperative concurrency.
// By contrast, operating system threads provide preemptive concurrency.
// The important different is that in cooperative concurrency, you have to
// cooperate. If one of your tasks causes a delay, such as by using
// std::thread::sleep or by performing significant CPU computation, it will
// not be interrupted.
// The upshot of this is that you should ensure you do not perform blocking
// calls inside your tasks. And if you have a CPU-intensive task to perform,
// it’s probably worth spawning an OS thread for it, or at least ensuring
// your executor will not starve your other tasks.
