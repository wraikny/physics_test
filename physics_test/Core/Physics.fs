namespace physics_test.core.physics


open physics_test.Math
open Box2DX

module Convert =
    let vec2 v =
        new Common.Vec2(v.x, v.y)


type WorldSetting =
    {
        lower_bound : Vec2F
        upper_bound : Vec2F
        gravity : Vec2F
        do_sleep : bool
        time_step : float32
        velocity_iterations : int
        position_iterations : int
    }


type World =
    {
        scale : float32
        setting : WorldSetting
        world : Dynamics.World
    }

module World =
    let create scale setting =
        let worldAABB =
            new Collision.AABB(
                LowerBound = Convert.vec2 (setting.lower_bound / scale),
                UpperBound = Convert.vec2 (setting.upper_bound / scale)
            )

        new Dynamics.World(worldAABB, setting.gravity |> Convert.vec2, setting.do_sleep)


    let init scale setting =
        {
            scale = scale
            setting = setting
            world = create scale setting
        }

    let run world =

        let ts = world.setting.time_step
        let v = world.setting.velocity_iterations
        let p = world.setting.position_iterations

        world.world.Step(ts, v, p)


type ObjectShape
    = Rectangle of Vec2F
    | Circle of float32
    //| Polygon of Vec2F list


type ObjectSetting =
    {
        position : Vec2F
        angle : float32
        density : float32 option
        friction : float32 option
        restitution : float32 option
        shape : ObjectShape
        is_set_mass_from_shapes : bool
    }


type Object =
    {
        body : Dynamics.Body
        shape : ObjectShape
    }

module Object =
    let create_shape (setting : ObjectSetting) (world : World) =
        let shape_def =
            setting.shape |> function
            | Rectangle size ->
                let shape_def = new Collision.PolygonDef()

                let size = size / 2.0f / world.scale

                shape_def.SetAsBox (size.x, size.y)

                shape_def:> Collision.ShapeDef

            | Circle radius ->
                new Collision.CircleDef(
                    Radius = radius / world.scale
                )
                :> Collision.ShapeDef

        setting.density |> function
        | None -> ()
        | Some density ->
            shape_def.Density <- density

        setting.friction |> function
        | None -> ()
        | Some frictionr ->
            shape_def.Friction <- frictionr

        setting.restitution |> function
        | None -> ()
        | Some restitution ->
            shape_def.Restitution <- restitution

        shape_def


    let create (world : World) (setting : ObjectSetting) =

        let shape_def =
            create_shape setting world

        
        let body =
            new Dynamics.BodyDef(
                Position = Convert.vec2 (setting.position / world.scale),
                Angle = (setting.angle |> Angle.degree_to_radian)
            )
            |> world.world.CreateBody

        shape_def
        |> body.CreateShape
        |> ignore

        if setting.is_set_mass_from_shapes then
            body.SetMassFromShapes()

        body


    let init world setting=
        {
            body = create world setting
            shape = setting.shape
        }