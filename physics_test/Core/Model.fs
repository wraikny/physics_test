﻿namespace physics_test.core.model

open physics_test
open physics_test.core

open System.Collections.Generic

type Model(scale, world_setting) =
    let world = physics.World.init scale world_setting

    let objects =
        new Dictionary<int, physics.Object>()

    let mutable next_id = 0

    let out_of_frame() =
        let destroy_pairs =
            objects
            |> Seq.filter(fun pair ->
                let object = pair.Value
                let lb = world.setting.lower_bound / world.scale
                let ub = world.setting.upper_bound / world.scale

                let size = object.shape |> function
                    | physics.Rectangle size ->
                        size
                    | physics.Circle radius ->
                        { Math.Vec2F.x = 1.0f; Math.Vec2F.y = 1.0f } * 2.0f * radius

                let rx, ry = size.x, size.y

                let p = object.body.GetPosition()

                (
                    (lb.x - rx < p.X && p.X < ub.x + rx)
                    && (lb.y - ry < p.Y && p.Y < ub.y + ry)
                ) |> not
            )
            |> Seq.toList

        for pair in destroy_pairs do
            let id = pair.Key
            let object = pair.Value
            world.world.DestroyBody object.body
            objects.Remove id |> ignore


    member this.World = world

    member this.NextID = next_id

    member this.add_object obj =
        let id = next_id
        if objects.ContainsKey id then
            false

        else
            objects.Add(id, obj)
            next_id <- id + 1

            true

    member this.remove_object id =
        objects.Remove id


    member this.get_object id =
        if objects.ContainsKey id then
            Some objects.[id]
        else
            None


    member this.run() =
        physics.World.run world

        out_of_frame()