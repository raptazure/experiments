use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use std::time::Duration;
use tokio::io;
use tokio::io::AsyncBufReadExt;
use tokio::net::{TcpListener, TcpStream};
use tokio::process::Command;
use tokio::task;
use tokio::time;

// avoid thread blocking by using Tokio's mutex
use tokio::sync::Mutex;

async fn take_break() -> Result<(), std::io::Error> {
    Command::new("date").spawn()?.await?;
    time::delay_for(Duration::from_secs(1)).await;
    Command::new("date").spawn()?.await?;
    time::delay_for(Duration::from_secs(1)).await;
    Command::new("date").spawn()?.await?;
    Ok(())
}

async fn dating() -> Result<(), std::io::Error> {
    loop {
        task::spawn_blocking(|| std::thread::sleep(Duration::from_secs(1))).await?;
        Command::new("date").spawn()?.await?;
    }
}

async fn copying() -> Result<(), std::io::Error> {
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    io::copy(&mut stdin, &mut stdout).await?;
    Ok(())
}

async fn networking() -> io::Result<()> {
    let mut listener = TcpListener::bind("127.0.0.1:8080").await?;
    let mut counter = 1u32;
    loop {
        let (socket, _) = listener.accept().await?;
        println!("Accepted connection #{}", counter);
        tokio::spawn(async move {
            match echo(socket).await {
                Ok(()) => println!("Connection #{} completed successfully", counter),
                Err(e) => println!("Connection #{} errored: {:?}", counter, e),
            }
        });
        counter += 1;
    }
}

async fn echo(socket: TcpStream) -> io::Result<()> {
    let (mut recv, mut send) = io::split(socket);
    io::copy(&mut recv, &mut send).await?;
    Ok(())
}

async fn http_client() -> io::Result<()> {
    let stream = TcpStream::connect("127.0.0.1:8080").await?;
    let (mut recv, mut send) = io::split(stream);

    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    // `move` to convince the compiler to make a `Future` that owns `stdin`
    let send = tokio::spawn(async move { io::copy(&mut stdin, &mut send).await });
    let recv = tokio::spawn(async move { io::copy(&mut recv, &mut stdout).await });

    send.await??;
    recv.await??;

    Ok(())
}

async fn play_lines() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    let _me = args.next(); // ignore command name
    let mut tasks = vec![];
    let count = Arc::new(Mutex::new(0u32));

    for filename in args {
        let count = count.clone();
        tasks.push(tokio::spawn(async move {
            let file = tokio::fs::File::open(filename).await?;
            let file = io::BufReader::new(file);
            let mut lines = file.lines();
            let mut local_count = 0u32;
            while let Some(_) = lines.next_line().await? {
                local_count += 1;
            }

            let mut count = count.lock().await;
            *count += local_count;
            Ok(()) as Result<(), std::io::Error>
        }));
    }

    for task in tasks {
        task.await??;
    }

    let count = count.lock().await;
    println!("Total lines: {}", *count);
    Ok(())
}

// LocalSet and !Send
async fn play_lines_local() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    let _me = args.next(); // ignore command name
    let mut tasks = vec![];
    let count = Rc::new(RefCell::new(0u32));

    for filename in args {
        let count = count.clone();

        // insist on spawning all our tasks on the same OS thread
        // and avoid the need for Send
        tasks.push(tokio::task::spawn_local(async move {
            let file = tokio::fs::File::open(filename).await?;
            let file = io::BufReader::new(file);
            let mut lines = file.lines();
            let mut local_count = 0u32;
            while let Some(_) = lines.next_line().await? {
                local_count += 1;
            }

            *count.borrow_mut() += local_count;
            Ok(()) as Result<(), std::io::Error>
        }))
    }

    for task in tasks {
        task.await??;
    }
    println!("Total lines: {}", count.borrow());

    Ok(())
}

// If you want to spawn onto the current thread, you need to set up your
// runtime to support that. And the way we do that is with LocalSet.
fn local_set() -> Result<(), std::io::Error> {
    let mut rt = tokio::runtime::Runtime::new()?;
    let local = tokio::task::LocalSet::new();
    local.block_on(&mut rt, play_lines_local())
}

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    for _ in 1..=10 {
        Command::new("echo").arg("Hello World").spawn()?.await?;
    }

    take_break().await?;

    let dating = task::spawn(dating());
    let copying = task::spawn(copying());

    dating.await??;
    copying.await??;

    networking().await?;
    http_client().await?;
    play_lines().await?;
    local_set()?;

    Ok(())
}
