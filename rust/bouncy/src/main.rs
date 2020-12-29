mod parse_args;

use self::parse_args::Frame;
use std::fmt::{Display, Formatter};

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

struct Game {
    frame: Frame,
    ball: Ball,
}

impl Game {
    fn new(frame: Frame) -> Game {
        let ball = Ball {
            x: 2,
            y: 4,
            vert_dir: VertDir::Up,
            horiz_dir: HorizDir::Left,
        };
        Game { frame, ball }
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

impl Display for Game {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        let top_bottom = |fmt: &mut Formatter| {
            write!(fmt, "+")?;
            for _ in 0..self.frame.width {
                write!(fmt, "-")?;
            }
            write!(fmt, "+\n")
        };

        top_bottom(fmt)?;

        for row in 0..self.frame.height {
            write!(fmt, "|")?;

            for column in 0..self.frame.width {
                let c = if row == self.ball.y && column == self.ball.x {
                    'o'
                } else {
                    ' '
                };
                write!(fmt, "{}", c)?;
            }

            write!(fmt, "|\n")?;
        }

        top_bottom(fmt)
    }
}

// fn main() -> Result<(), self::parse_args::ParseError> {
fn main() {
    let window = pancurses::initscr();
    let (max_y, max_x) = window.get_max_yx();
    let frame = Frame {
        width: max_x as u32 - 4,
        height: max_y as u32 - 4,
    };
    // let frame = parse_args::parse_args()?;
    let mut game = Game::new(frame);
    let sleep_duration = std::time::Duration::from_millis(33);
    loop {
        window.clear();
        window.printw(game.to_string());
        window.refresh();
        game.step();
        std::thread::sleep(sleep_duration);
    }
}
