use pancurses::{endwin, initscr, Input, Window};

enum VertDir {
    Up,
    Down,
}

enum HorizDir {
    Left,
    Right,
}

struct Ball {
    x: u32,
    y: u32,
    vert_dir: VertDir,
    horiz_dir: HorizDir,
}

struct Frame {
    width: u32,
    height: u32,
}

struct Game {
    frame: Frame,
    ball: Ball,
}

impl Game {
    fn new(window: &Window) -> Result<Game, String> {
        let (max_y, max_x) = window.get_max_yx();
        if max_y < 10 || max_x < 10 {
            return Err(String::from("Window is too small, exiting"));
        }
        let frame = Frame {
            width: max_x as u32 - 2,
            height: max_y as u32 - 2,
        };
        let ball = Ball {
            x: 2,
            y: 4,
            vert_dir: VertDir::Up,
            horiz_dir: HorizDir::Left,
        };
        Ok(Game { frame, ball })
    }

    fn step(&mut self) {
        self.ball.bounce(&self.frame);
        self.ball.mv();
    }
}

impl Ball {
    fn bounce(&mut self, frame: &Frame) {
        if self.x == 0 {
            self.horiz_dir = HorizDir::Right;
        } else if self.x == frame.width - 1 {
            self.horiz_dir = HorizDir::Left;
        }

        if self.y == 0 {
            self.vert_dir = VertDir::Down;
        } else if self.y == frame.height - 1 {
            self.vert_dir = VertDir::Up;
        }
    }

    fn mv(&mut self) {
        match self.horiz_dir {
            HorizDir::Left => self.x -= 1,
            HorizDir::Right => self.x += 1,
        }
        match self.vert_dir {
            VertDir::Up => self.y -= 1,
            VertDir::Down => self.y += 1,
        }
    }
}

// no longer need: impl Display for Game

fn main() -> Result<(), String> {
    let window = initscr();

    // set the timeout on input to our 33 milliseconds
    window.timeout(33);

    let mut game = Game::new(&window)?;

    loop {
        window.clear(); // get rid of old content
        window.border(
            '|', // left
            '|', // right
            '-', // top
            '-', // bottom
            '+', // top left
            '+', // top right
            '+', // bottom left
            '+', // bottom right
        );

        // put the ball on the screen. Add 1 to the x and y to account
        // for the border
        window.mvaddch(game.ball.y as i32 + 1, game.ball.x as i32 + 1, 'o');

        // move the cursor to the top left so it's out of the way
        window.mv(0, 0);

        window.refresh(); // update the screen

        // get the next bit of input
        match window.getch() {
            // exit on a q
            Some(Input::Character('q')) => {
                endwin();
                println!("Thanks for playing!");
                return Ok(());
            }

            // window size changed
            Some(Input::KeyResize) => {
                // resetting the game. We _could_ do some logic of
                // keeping the old ball position and check if it's
                // outside of the new window, but I'm a lazy Haskeller
                game = Game::new(&window)?;
            }

            // something else happened, just step
            _ => {
                game.step();
            }
        }
    }
}
