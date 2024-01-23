use leaf::annotations::Symbolizable;

fn main() {
    let array = [
        Location::Area(Area {
            lines: &[
                Line {
                    points: [Point(0, 0), Point(1, 0)],
                },
                Line {
                    points: [Point(1, 0), Point(1, 1)],
                },
                Line {
                    points: [Point(1, 1), Point(0, 0)],
                },
            ],
        }),
        Location::Point(Point(0, 0)),
    ];
    let i = 0.mark_symbolic();

    let location = &array[i];
    match location {
        Location::Area(area) => {
            let lines = area.lines;
            let j = 0.mark_symbolic();
            let line = &lines[j];
            let k = 0.mark_symbolic();
            let point = &line.points[k];

            if point.0 >= 1 && point.1 >= 1 {
                foo();
            }
        }
        _ => {}
    }
}

fn foo() {}

enum Location<'a> {
    Area(Area<'a>),
    Point(Point),
}

struct Area<'a> {
    lines: &'a [Line],
}

struct Line {
    points: [Point; 2],
}

struct Point(i32, i32);
