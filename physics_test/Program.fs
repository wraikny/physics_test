namespace physics_test

open physics_test
open physics_test.Math

type MainScene() =
    inherit asd.Scene()

    let model =
        let setting : core.physics.WorldSetting =
            {
                lower_bound = Vec2F.init(0.0f, 0.0f)
                upper_bound = Vec2F.init(800.0f, 600.0f)
                gravity = Vec2F.init(0.0f, 10.0f)
                do_sleep = true
                time_step = 1.0f / 60.0f
                velocity_iterations = 8
                position_iterations = 1
            }

        new core.model.Model(50.0f, setting)


    let add_ground() =
        core.physics.Object.init model.World
            {
                position = Vec2F.init(400.0f, 500.0f)
                angle = 0.0f
                density = None
                friction = None
                restitution = None
                shape = core.physics.Rectangle(Vec2F.init(600.0f, 30.0f))
                is_set_mass_from_shapes = false
            }
        |> model.add_object
        |> ignore


    let add_object() =

        let setting : core.physics.ObjectSetting =
            {
                position =
                    let p = asd.Engine.Mouse.Position
                    Vec2F.init(p.X, p.Y)
                angle = 0.0f
                density = Some 1.0f
                friction = Some 0.3f
                restitution = Some 0.1f
                shape = core.physics.Rectangle(Vec2F.init(50.0f, 50.0f))
                is_set_mass_from_shapes = true
            }

        let KeyPush =
            asd.Engine.Keyboard.GetKeyState
            >> (=) asd.KeyState.Push

        if KeyPush asd.Keys.Z then
            
            core.physics.Object.init model.World
                setting
            |> model.add_object
            |> ignore


        if KeyPush asd.Keys.X then
            
            core.physics.Object.init model.World
                { setting with
                    shape = core.physics.Circle 25.0f
                }
            |> model.add_object
            |> ignore

    let layer = new view.MainLayer(model)


    override this.OnRegistered() =
        this.AddLayer layer
        add_ground()


    override this.OnUpdated() =
        model.run()

        add_object()


module Program =
    [<EntryPoint>]
    let main argv =
        ("Physics", 800, 600, new asd.EngineOption())
        |> asd.Engine.Initialize
        |> ignore

        new MainScene()
        |> asd.Engine.ChangeScene

        while asd.Engine.DoEvents() do
            asd.Engine.Update()

        asd.Engine.Terminate()

        0
