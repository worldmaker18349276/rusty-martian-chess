mod model {
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
    struct Playfield {
        board : [[Option<Chess>; 4]; 8],
        scores : [i32; 2],
        turn : Player,
    }
}

fn main() {
    println!("Hello, world!");
}
