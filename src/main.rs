mod model {
    use std::fmt;
    use std::ops::Add;
    use std::vec::Vec;

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Chess {
        Pawn,
        Drone,
        Queen,
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Player {
        Player1,
        Player2,
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct Playfield {
        board : [[Option<Chess>; 4]; 8],
        scores : [i32; 2],
        turn : Player,
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct Point(pub i32, pub i32);

    impl Add<Point> for Point {
        type Output = Point;

        fn add(self, other: Point) -> Point {
            Point(self.0 + other.0, self.1 + other.1)
        }
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Action {
        Move {
            start : Point,
            goal : Point,
        },
        Capture {
            start : Point,
            goal : Point,
        },
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct OutOfBounds(Point);

    impl Chess {
        fn point(&self) -> i32 {
            match self {
                Chess::Pawn => 1,
                Chess::Drone => 2,
                Chess::Queen => 3,
            }
        }
    }

    impl Player {
        fn index(&self) -> i32 {
            match self {
                Player::Player1 => 0,
                Player::Player2 => 1,
            }
        }

        fn next(&self) -> Player {
            match self {
                Player::Player1 => Player::Player2,
                Player::Player2 => Player::Player1,
            }
        }
    }

    impl Playfield {
        pub fn init() -> Playfield {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            Playfield {
                board : [
                    [q, q, d, n],
                    [q, d, p, n],
                    [d, p, p, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, p, d],
                    [n, p, d, q],
                    [n, d, q, q],
                ],
                scores : [0, 0],
                turn : Player::Player1,
            }
        }

        pub fn get(&self, point: Point) -> Result<(Player, Option<Chess>), OutOfBounds> {
            if point.0 >= 0 && point.0 < 4 && point.1 >= 0 && point.1 < 4 {
                Ok((Player::Player1, self.board[point.0 as usize][point.1 as usize]))
            } else if point.0 >= 4 && point.0 < 8 && point.1 >= 0 && point.1 < 4 {
                Ok((Player::Player2, self.board[point.0 as usize][point.1 as usize]))
            } else {
                Err(OutOfBounds(point))
            }
        }

        pub fn possible_moves(&self, start: Point) -> Vec<Action> {
            let mut vec = Vec::<Action>::new();
            let orths: &'static _ = &[
                Point( 1, 0),
                Point(-1, 0),
                Point( 0, 1),
                Point( 0,-1),
            ];
            let diags: &'static _ = &[
                Point( 1, 1),
                Point( 1,-1),
                Point(-1, 1),
                Point(-1,-1),
            ];

            match self.get(start) {
                Err(_) | Ok((_, Option::None)) => {
                    vec
                },

                Ok((player, _)) if player != self.turn => {
                    vec
                },

                Ok((_, Option::Some(Chess::Pawn))) => {
                    for &dir in diags.iter() {
                        let goal = start + dir;
                        match self.get(goal) {
                            Err(_) => {},
                            Ok((owner, Option::Some(_))) => {
                                if owner != self.turn {
                                    vec.push(Action::Capture { start, goal });
                                }
                                // TODO: field promotion
                            },
                            Ok((_, Option::None)) => {
                                vec.push(Action::Move { start, goal });
                            },
                        }
                    }
                    vec
                },

                Ok((_, Option::Some(Chess::Drone))) => {
                    for &dir in orths.iter() {
                        let mut goal = start + dir;
                        match self.get(goal) {
                            Err(_) => {},
                            Ok((owner, Option::Some(_))) => {
                                if owner != self.turn {
                                    vec.push(Action::Capture { start, goal });
                                }
                                // TODO: field promotion
                            },
                            Ok((_, Option::None)) => {
                                vec.push(Action::Move { start, goal });

                                goal = goal + dir;
                                match self.get(goal) {
                                    Err(_) => {},
                                    Ok((owner, Option::Some(_))) => {
                                        if owner != self.turn {
                                            vec.push(Action::Capture { start, goal });
                                        }
                                        // TODO: field promotion
                                    },
                                    Ok((_, Option::None)) => {
                                        vec.push(Action::Move { start, goal });
                                    },
                                }
                            },
                        }
                    }
                    vec
                },

                Ok((_, Option::Some(Chess::Queen))) => {
                    for &dir in orths.iter().chain(diags.iter()) {
                        let mut goal = start;
                        loop {
                            goal = goal + dir;
                            match self.get(goal) {
                                Err(_) => {},
                                Ok((owner, Option::Some(_))) => {
                                    if owner != self.turn {
                                        vec.push(Action::Capture { start, goal });
                                    }
                                    // TODO: field promotion
                                },
                                Ok((_, Option::None)) => {
                                    vec.push(Action::Move { start, goal });
                                    continue;
                                },
                            }
                            break;
                        }
                    }
                    vec
                },
            }
        }
    }

    impl fmt::Display for Playfield {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            for line in self.board.iter() {
                for grid in line.iter() {
                    match grid {
                        Option::None => write!(f, ".")?,
                        Option::Some(Chess::Pawn) => write!(f, "p")?,
                        Option::Some(Chess::Drone) => write!(f, "d")?,
                        Option::Some(Chess::Queen) => write!(f, "q")?,
                    }
                    write!(f, " ")?;
                }
                write!(f, "\n")?;
            }
            Ok(())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn point_add() {
            assert_eq!(Point(1, 2) + Point(3, 4), Point(4, 6));
            assert_eq!(Point(1, -2) + Point(-3, 4), Point(-2, 2));
        }

        #[test]
        fn playfield_get_owner() {
            let playfield = Playfield::init();

            for x in 0..4 {
                for y in 0..4 {
                    assert!(
                        match playfield.get(Point(x, y)) {
                            Ok((Player::Player1, _)) => true,
                            _ => false,
                        }
                    );
                }
            }

            for x in 4..8 {
                for y in 0..4 {
                    assert!(
                        match playfield.get(Point(x, y)) {
                            Ok((Player::Player2, _)) => true,
                            _ => false,
                        }
                    );
                }
            }
        }

        #[test]
        fn playfield_get_out_of_bounds() {
            let playfield = Playfield::init();

            assert_eq!(playfield.get(Point(-1, 3)), Err(OutOfBounds(Point(-1, 3))));
            assert_eq!(playfield.get(Point(9, 3)), Err(OutOfBounds(Point(9, 3))));
            assert_eq!(playfield.get(Point(2, -2)), Err(OutOfBounds(Point(2, -2))));
            assert_eq!(playfield.get(Point(2, 4)), Err(OutOfBounds(Point(2, 4))));
        }

        #[test]
        fn playfield_possible_moves_pawn() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board : [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, n, n],
                    [n, n, p, n],
                    [n, d, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                ],
                scores : [0, 0],
                turn : Player::Player1,
            };
            let mut moves = playfield.possible_moves(Point(3, 2));
            let mut expected = vec![
                Action::Move {start: Point(3, 2), goal: Point(2, 3)},
                Action::Move {start: Point(3, 2), goal: Point(4, 3)}, // cross canal
                // Action::Move {start: Point(3, 2), goal: Point(2, 1)}, // stop by chess
                Action::Capture {start: Point(3, 2), goal: Point(4, 1)}, // capture chess in the enemy's territory
            ];
            moves.sort();
            expected.sort();
            assert_eq!(moves, expected);
        }

        #[test]
        fn playfield_possible_moves_drone() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board : [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, d, n],
                    [n, n, n, n],
                    [n, n, q, n],
                    [n, n, n, n],
                    [n, n, n, n],
                ],
                scores : [0, 0],
                turn : Player::Player1,
            };
            let mut moves = playfield.possible_moves(Point(3, 2));
            let mut expected = vec![
                Action::Move {start: Point(3, 2), goal: Point(2, 2)},
                Action::Move {start: Point(3, 2), goal: Point(1, 2)}, // cross an empty grid
                Action::Move {start: Point(3, 2), goal: Point(3, 3)},
                // Action::Move {start: Point(3, 2), goal: Point(3, 4)}, // stop by boundary
                // Action::Capture {start: Point(3, 2), goal: Point(3, 1)}, // stop by chess
                // Action::Capture {start: Point(3, 2), goal: Point(3, 0)}, // stop by chess
                Action::Move {start: Point(3, 2), goal: Point(4, 2)}, // cross canal
                Action::Capture {start: Point(3, 2), goal: Point(5, 2)}, // capture chess in the enemy's territory
            ];
            moves.sort();
            expected.sort();
            assert_eq!(moves, expected);
        }

        #[test]
        fn playfield_possible_moves_queen() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board : [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [p, n, q, n],
                    [n, n, n, n],
                    [p, n, n, n],
                    [n, n, n, n],
                    [n, n, d, n],
                ],
                scores : [0, 0],
                turn : Player::Player1,
            };
            let mut moves = playfield.possible_moves(Point(3, 2));
            let mut expected = vec![
                Action::Move {start: Point(3, 2), goal: Point(2, 2)},
                Action::Move {start: Point(3, 2), goal: Point(1, 2)}, // cross an empty grid
                Action::Move {start: Point(3, 2), goal: Point(0, 2)}, // cross two empty grids
                Action::Move {start: Point(3, 2), goal: Point(3, 3)},
                // Action::Move {start: Point(3, 2), goal: Point(3, 4)}, // stop by boundary
                Action::Move {start: Point(3, 2), goal: Point(3, 1)},
                // Action::Move {start: Point(3, 2), goal: Point(3, 0)}, // stop by chess
                Action::Move {start: Point(3, 2), goal: Point(4, 2)}, // cross canal
                Action::Move {start: Point(3, 2), goal: Point(5, 2)}, // cross an empty grid
                Action::Move {start: Point(3, 2), goal: Point(6, 2)}, // cross two empty grids
                Action::Capture {start: Point(3, 2), goal: Point(7, 2)}, // capture chess in the enemy's territory

                Action::Move {start: Point(3, 2), goal: Point(2, 1)}, // diagonal moves
                Action::Move {start: Point(3, 2), goal: Point(1, 0)}, // diagonal moves
                Action::Move {start: Point(3, 2), goal: Point(2, 3)}, // diagonal moves
                Action::Move {start: Point(3, 2), goal: Point(4, 3)}, // diagonal moves
                Action::Move {start: Point(3, 2), goal: Point(4, 1)}, // diagonal moves
                Action::Capture {start: Point(3, 2), goal: Point(5, 0)}, // diagonal capture
            ];
            moves.sort();
            expected.sort();
            assert_eq!(moves, expected);
        }
    }
}

fn main() {
    let playfield = model::Playfield::init();
    println!("{}", playfield);
}
