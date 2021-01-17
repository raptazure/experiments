use gio::prelude::*;
use gtk::prelude::*;
use std::cell::RefCell;
use std::error::Error;
use std::io::Write;
use std::rc::Rc;

use gtk::{Application, ApplicationWindow, Button};

fn main() -> Result<(), Box<dyn Error>> {
    let application =
        Application::new(Some("com.github.gtk-rs.examples.basic"), Default::default())
            .expect("failed to init GTK app");

    let file = std::fs::File::create("mylog.txt")?;
    let file = Rc::new(RefCell::new(file));

    application.connect_activate(move |app| {
        let window = ApplicationWindow::new(app);
        window.set_title("First GTK pogram");
        window.set_default_size(350, 70);

        let button = Button::new_with_label("Click me!");

        let file = file.clone();
        button.connect_clicked(
            move |_| match file.borrow_mut().write_all(b"I was clicked.\n") {
                Ok(()) => (),
                Err(e) => eprintln!("Error writing to file: {}", e),
            },
        );
        window.add(&button);

        window.show_all();
    });

    application.run(&[]);
    Ok(())
}
