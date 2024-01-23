use leaf::annotations::Symbolizable;

fn main() {
    let array = [
        Area {
            lines: &[
                Line([Point(64, 256), Point(65, 256)]),
                Line([Point(65, 256), Point(65, 257)]),
                Line([Point(65, 257), Point(64, 256)]),
            ],
        },
        Area {
            lines: &[
                Line([Point(63, 255), Point(62, 255)]),
                Line([Point(62, 255), Point(62, 254)]),
                Line([Point(62, 254), Point(63, 255)]),
            ],
        },
    ];
    let i = 1.mark_symbolic();

    let area = &array[i];
    {
        let lines = area.lines;
        let j = 0.mark_symbolic();
        let line = &lines[j];
        let k = 0.mark_symbolic();
        let point = &line.0[k];

        if (point.0 >= 65) & (point.1 >= 257) {
            foo();
        }
    }
}

fn foo() {}

struct Area<'a> {
    lines: &'a [Line],
}

struct Line([Point; 2]);

struct Point(i32, i32);
