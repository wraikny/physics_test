namespace physics_test.view

open physics_test
open physics_test.Math

module Convert =
    let vec2 v =
        new asd.Vector2DF(v.x, v.y)

    let from_box2d (v : Box2DX.Common.Vec2) =
        new asd.Vector2DF(v.X, v.Y)


type Object(id : int, model : core.model.Model) =
    inherit asd.GeometryObject2D()

    let id = id
    let model = model

    let object() =
        model.get_object id


    member this.SetShape (obj : core.physics.Object) =
        this.Shape <-
            obj.shape |> function
            | core.physics.Rectangle size ->
                let size = Convert.vec2 size
                new asd.RectangleShape(
                    DrawingArea =
                        new asd.RectF(-size / 2.0f, size)
                ) :> asd.Shape

            | core.physics.Circle radius ->
                new asd.CircleShape(
                    OuterDiameter = radius * 2.0f
                ) :> asd.Shape
        

    member this.Transform (obj : core.physics.Object) =
        this.Position <-
            (obj.body.GetPosition()
            |> Convert.from_box2d)
            * model.World.scale

        this.Angle <-
            obj.body.GetAngle()
            |> Math.Angle.radian_to_degree



    override this.OnAdded() =
        object() |> function
        | None -> ()
        | Some obj ->
            this.SetShape obj
            this.Transform obj

    
    override this.OnUpdate() =
        object() |> function
        | None ->
            this.Dispose()

        | Some obj ->
            this.Transform obj


type MainLayer(model : core.model.Model) =
    inherit asd.Layer2D()
    let model = model

    let mutable next_id = 0

    member this.RegisterObjects() =
        while next_id < model.NextID do
            new Object(next_id, model)
            |> this.AddObject

            next_id <- next_id + 1

    override this.OnAdded() =
        this.RegisterObjects()

    override this.OnUpdated() =
        this.RegisterObjects()