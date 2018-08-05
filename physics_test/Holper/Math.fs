namespace physics_test

module Math =
    let pi = 3.14159265f

    let degree_to_radian d =
        d / 180.0f * pi

    let radian_to_degree d =
        d * 180.0f / pi

    type Vec2F =
        {
            x : float32
            y : float32
        }

        static member (~-) (a) =
            { x = -a.x; y = -a.y }

        static member (+) (a, b) =
            { x = a.x + b.x; y = a.y + b.y }

        static member (-) (a, b) =
            { x = a.x - b.x; y = a.y - b.y }

        static member (*) (a, b) =
            { x = a.x * b; y = a.y * b }

        static member (*) (a, b) =
            { x = a * b.x; y = a * b.y }

        static member (/) (a, b) =
            { x = a.x / b; y = a.y / b }

        static member init (x, y) =
            { x = x; y = y }