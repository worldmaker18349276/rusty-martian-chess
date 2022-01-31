mod model {
    use std::fmt;

    #[derive(Debug, Copy, Clone)]
    enum Chess {
        Pawn,
        Drone,
        Queen,
    }

    #[derive(Debug, Copy, Clone)]
    enum Player {
        Player1,
        Player2,
    }

    #[derive(Debug, Copy, Clone)]
    pub struct Playfield {
        board : [[Option<Chess>; 4]; 8],
        scores : [i32; 2],
        turn : Player,
    }

    #[derive(Debug, Copy, Clone)]
    struct Point(usize, usize);

    #[derive(Debug, Copy, Clone)]
    enum Action {
        Move {
            start : Point,
            goal : Point,
        },
        Capture {
            start : Point,
            goal : Point,
        },
    }

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

        pub fn is_in_territory(&self, point: Point) -> bool {
            match self {
                Player::Player1 => point.1 < 4 && point.0 < 4,
                Player::Player2 => point.1 < 4 && point.0 >= 4 && point.0 < 8,
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

        pub fn is_in_board(&self, point: Point) -> bool {
            point.1 < 4 && point.0 < 8 
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
        fn player_is_in_territory() {
            let player1 = Player::Player1;
            let player2 = Player::Player2;

            for x in 0..4 {
                for y in 0..4 {
                    assert_eq!(player1.is_in_territory(Point(x, y)), true);
                    assert_eq!(player2.is_in_territory(Point(x, y)), false);
                }
            }

            for x in 4..8 {
                for y in 0..4 {
                    assert_eq!(player1.is_in_territory(Point(x, y)), false);
                    assert_eq!(player2.is_in_territory(Point(x, y)), true);
                }
            }
        }

        #[test]
        fn player_out_of_bounds() {
            let player1 = Player::Player1;
            let player2 = Player::Player2;

            assert_eq!(player1.is_in_territory(Point(9, 3)), false);
            assert_eq!(player2.is_in_territory(Point(9, 3)), false);
            assert_eq!(player1.is_in_territory(Point(2, 4)), false);
            assert_eq!(player2.is_in_territory(Point(2, 4)), false);
        }

        #[test]
        fn playfield_is_in_board() {
            let playfield = Playfield::init();

            for x in 0..8 {
                for y in 0..4 {
                    assert_eq!(playfield.is_in_board(Point(x, y)), true);
                }
            }
        }

        #[test]
        fn playfield_out_of_bounds() {
            let playfield = Playfield::init();

            assert_eq!(playfield.is_in_board(Point(9, 3)), false);
            assert_eq!(playfield.is_in_board(Point(2, 4)), false);
        }
    }
}

fn main() {
    let playfield = model::Playfield::init();
    println!("{}", playfield);
}
