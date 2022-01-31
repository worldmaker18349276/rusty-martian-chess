#[allow(unused)]

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
        board: [[Option<Chess>; 4]; 8],
        scores: [i32; 2],
        turn: Player,
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
    pub enum ChessMovement {
        PawnUpLeft,
        PawnUpRight,
        PawnDownLeft,
        PawnDownRight,

        DroneUp,
        DroneUp2,
        DroneDown,
        DroneDown2,
        DroneLeft,
        DroneLeft2,
        DroneRight,
        DroneRight2,

        QueenUp(usize),
        QueenDown(usize),
        QueenLeft(usize),
        QueenRight(usize),
        QueenUpLeft(usize),
        QueenUpRight(usize),
        QueenDownLeft(usize),
        QueenDownRight(usize),
    }

    impl ChessMovement {
        fn value(&self) -> (Chess, Point, usize) {
            match self {
                ChessMovement::PawnUpLeft    => (Chess::Pawn, Point(-1,-1), 0),
                ChessMovement::PawnUpRight   => (Chess::Pawn, Point(-1, 1), 0),
                ChessMovement::PawnDownLeft  => (Chess::Pawn, Point( 1,-1), 0),
                ChessMovement::PawnDownRight => (Chess::Pawn, Point( 1, 1), 0),
        
                ChessMovement::DroneUp     => (Chess::Drone, Point(-1, 0), 0),
                ChessMovement::DroneUp2    => (Chess::Drone, Point(-1, 0), 1),
                ChessMovement::DroneDown   => (Chess::Drone, Point( 1, 0), 0),
                ChessMovement::DroneDown2  => (Chess::Drone, Point( 1, 0), 1),
                ChessMovement::DroneLeft   => (Chess::Drone, Point( 0,-1), 0),
                ChessMovement::DroneLeft2  => (Chess::Drone, Point( 0,-1), 1),
                ChessMovement::DroneRight  => (Chess::Drone, Point( 0, 1), 0),
                ChessMovement::DroneRight2 => (Chess::Drone, Point( 0, 1), 1),
        
                ChessMovement::QueenUp(cross)        => (Chess::Queen, Point(-1, 0), *cross),
                ChessMovement::QueenDown(cross)      => (Chess::Queen, Point( 1, 0), *cross),
                ChessMovement::QueenLeft(cross)      => (Chess::Queen, Point( 0,-1), *cross),
                ChessMovement::QueenRight(cross)     => (Chess::Queen, Point( 0, 1), *cross),
                ChessMovement::QueenUpLeft(cross)    => (Chess::Queen, Point(-1,-1), *cross),
                ChessMovement::QueenUpRight(cross)   => (Chess::Queen, Point(-1, 1), *cross),
                ChessMovement::QueenDownLeft(cross)  => (Chess::Queen, Point( 1,-1), *cross),
                ChessMovement::QueenDownRight(cross) => (Chess::Queen, Point( 1, 1), *cross),
            }
        }

        fn possible_actions_of(chess: &Chess) -> Vec<Self> {
            match chess {
                Chess::Pawn => vec![
                    ChessMovement::PawnUpLeft,
                    ChessMovement::PawnUpRight,
                    ChessMovement::PawnDownLeft,
                    ChessMovement::PawnDownRight,
                ],
                Chess::Drone => vec![
                    ChessMovement::DroneUp,
                    ChessMovement::DroneUp2,
                    ChessMovement::DroneDown,
                    ChessMovement::DroneDown2,
                    ChessMovement::DroneLeft,
                    ChessMovement::DroneLeft2,
                    ChessMovement::DroneRight,
                    ChessMovement::DroneRight2,
                ],
                Chess::Queen => {
                    let mut res = Vec::new();
                    for cross in 0..8 {
                        res.push(ChessMovement::QueenUp(cross));
                        res.push(ChessMovement::QueenDown(cross));
                        res.push(ChessMovement::QueenLeft(cross));
                        res.push(ChessMovement::QueenRight(cross));
                        res.push(ChessMovement::QueenUpLeft(cross));
                        res.push(ChessMovement::QueenUpRight(cross));
                        res.push(ChessMovement::QueenDownLeft(cross));
                        res.push(ChessMovement::QueenDownRight(cross));
                    }
                    res
                },
            }
        }
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct Action {
        player: Player,
        position: Point,
        movement: ChessMovement,
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
                board: [
                    [q, q, d, n],
                    [q, d, p, n],
                    [d, p, p, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, p, d],
                    [n, p, d, q],
                    [n, d, q, q],
                ],
                scores: [0, 0],
                turn: Player::Player1,
            }
        }

        pub fn get(&self, position: Point) -> Result<(Player, Option<Chess>), OutOfBounds> {
            if position.0 >= 0 && position.0 < 4 && position.1 >= 0 && position.1 < 4 {
                Ok((Player::Player1, self.board[position.0 as usize][position.1 as usize]))
            } else if position.0 >= 4 && position.0 < 8 && position.1 >= 0 && position.1 < 4 {
                Ok((Player::Player2, self.board[position.0 as usize][position.1 as usize]))
            } else {
                Err(OutOfBounds(position))
            }
        }

        pub fn is_valid_move(&self, position: Point, movement: ChessMovement) -> bool {
            let (chess, dir, cross) = movement.value();
            if let Ok((player, Option::Some(chess_))) = self.get(position) {
                if chess_ != chess {
                    return false;
                }

                // TODO: no rejection
                
                let mut goal = position;
                for _ in 0..cross {
                    goal = goal + dir;
                    if let Ok((_, Option::None)) = self.get(goal) {
                        continue;
                    } else {
                        return false;
                    }
                }

                goal = goal + dir;
                match self.get(goal) {
                    Err(_) => false,
                    Ok((_, Option::None)) => true,
                    Ok((owner, Option::Some(_))) => {
                        if owner != player {
                            return true;
                        } else {
                            // TODO: field promotion
                            return false;
                        }
                    },
                }
            } else {
                return false;
            }
        }

        pub fn possible_actions(&self, position: Point) -> Vec<Action> {
            let mut vec = Vec::<Action>::new();

            if let Ok((player, Option::Some(chess))) = self.get(position) {
                for movement in ChessMovement::possible_actions_of(&chess) {
                    if self.is_valid_move(position, movement) {
                        vec.push(Action { player, position, movement });
                    }
                }
                return vec;
            } else {
                return vec;
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
        fn playfield_possible_actions_invalid() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board: [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, p],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, p],
                    [n, n, n, n],
                ],
                scores: [0, 0],
                turn: Player::Player1,
            };

            let playfield2 = Playfield {
                board: [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, p],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, p],
                    [n, n, n, n],
                ],
                scores: [0, 0],
                turn: Player::Player2,
            };

            assert_eq!(playfield.possible_actions(Point(3, 2)), vec![]); // empty grid
            assert_eq!(playfield.possible_actions(Point(2, 5)), vec![]); // out of bounds
            assert_ne!(playfield.possible_actions(Point(6, 3)), vec![]); // not restricted by turn
            assert_ne!(playfield2.possible_actions(Point(2, 3)), vec![]); // not restricted by turn
        }

        #[test]
        fn playfield_possible_actions_pawn() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board: [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, n, n],
                    [n, n, p, n],
                    [n, d, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                ],
                scores: [0, 0],
                turn: Player::Player1,
            };
            let mut moves = playfield.possible_actions(Point(3, 2));
            let mut expected = vec![
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::PawnUpRight},
                // cross canal
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::PawnDownRight},
                // // stop by chess
                // Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::PawnUpLeft},
                // capture chess in the enemy's territory
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::PawnDownLeft},
            ];
            moves.sort();
            expected.sort();
            assert_eq!(moves, expected);
        }

        #[test]
        fn playfield_possible_actions_drone() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board: [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, d, n],
                    [n, n, n, n],
                    [n, n, q, n],
                    [n, n, n, n],
                    [n, n, n, n],
                ],
                scores: [0, 0],
                turn: Player::Player1,
            };
            let mut moves = playfield.possible_actions(Point(3, 2));
            let mut expected = vec![
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::DroneUp},
                // cross an empty grid
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::DroneUp2},
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::DroneRight},
                // // stop by boundary
                // Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::DroneRight2},
                // // stop by chess
                // Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::DroneLeft},
                // Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::DroneLeft2},
                // cross canal
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::DroneDown},
                // capture chess in the enemy's territory
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::DroneDown2},
            ];
            moves.sort();
            expected.sort();
            assert_eq!(moves, expected);
        }

        #[test]
        fn playfield_possible_actions_queen() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board: [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [p, n, q, n],
                    [n, n, n, n],
                    [p, n, n, n],
                    [n, n, n, n],
                    [n, n, d, n],
                ],
                scores: [0, 0],
                turn: Player::Player1,
            };
            let mut moves = playfield.possible_actions(Point(3, 2));
            let mut expected = vec![
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenUp(0)},
                // cross an empty grid
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenUp(1)},
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenUp(2)},
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenRight(0)},
                // // stop by boundary
                // Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenRight(1)},
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenLeft(0)},
                // // stop by chess
                // Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenRight(1)},
                // cross canal
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenDown(0)},
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenDown(1)},
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenDown(2)},
                // capture chess in the enemy's territory
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenDown(3)},

                // diagonal moves
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenUpLeft(0)},
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenUpLeft(1)},
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenUpRight(0)},
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenDownRight(0)},
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenDownLeft(0)},
                // diagonal capture
                Action {player: Player::Player1, position: Point(3, 2), movement: ChessMovement::QueenDownLeft(1)},
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
