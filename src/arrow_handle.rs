use bevy::{
    color::palettes::css::*,
    prelude::*,
    sprite::{Material2d, MaterialMesh2dBundle, Mesh2dHandle},
};
use bevy_mod_picking::prelude::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Component)]
pub enum CurveHandle {
    Position0,
    Velocity0,
    Acceleration0,
    Position1,
    Velocity1,
    Acceleration1,
}

pub struct HandleBundleFactory<M: Material2d> {
    shape_handle: Mesh2dHandle,
    shape_material: Handle<M>,
}

impl<M: Material2d> HandleBundleFactory<M> {
    pub fn new(shape_handle: Mesh2dHandle, shape_material: Handle<M>) -> Self {
        Self {
            shape_handle,
            shape_material,
        }
    }

    pub fn new_handle(&self, pos: Vec2) -> HandleBundle<M> {
        HandleBundle::new(
            pos,
            &self.shape_handle,
            &self.shape_material,
            On::<Pointer<Drag>>::target_component_mut::<Transform>(|drag, transform| {
                transform.translation += Vec2 {
                    y: -drag.delta.y,
                    ..drag.delta
                }
                .extend(0.0)
                    / 128.0;
            }),
        )
    }

    pub fn new_child_handle(&self, pos: Vec2) -> HandleBundle<M> {
        HandleBundle::new(
            pos,
            &self.shape_handle,
            &self.shape_material,
            On::<Pointer<Drag>>::run(|_: ()| {}),
        )
    }
}

impl HandleBundleFactory<ColorMaterial> {
    pub fn setup(meshes: &mut Assets<Mesh>, materials: &mut Assets<ColorMaterial>) -> Self {
        let circle_mesh_handle = meshes.add(Circle::new(16.0_f32.recip())).into();
        let color_mat_handle = materials.add(ColorMaterial::from_color(WHITE));
        Self::new(circle_mesh_handle, color_mat_handle)
    }
}

#[derive(Bundle)]
pub struct HandleBundle<M: Material2d> {
    mesh_2d_bundle: MaterialMesh2dBundle<M>,
    pickable: PickableBundle,
    on_drag_start: On<Pointer<DragStart>>,
    on_drag_end: On<Pointer<DragEnd>>,
    on_drag: On<Pointer<Drag>>,
}

impl<M: Material2d> HandleBundle<M> {
    fn new(
        pos: Vec2,
        shape: &Mesh2dHandle,
        material: &Handle<M>,
        on_drag: On<Pointer<Drag>>,
    ) -> Self {
        Self {
            mesh_2d_bundle: MaterialMesh2dBundle {
                transform: Transform::from_translation(pos.extend(0.0)),
                mesh: shape.clone(),
                material: material.clone(),
                ..default()
            },
            pickable: PickableBundle::default(),
            on_drag_start: On::<Pointer<DragStart>>::target_insert(Pickable::IGNORE),
            on_drag_end: On::<Pointer<DragEnd>>::target_insert(Pickable::default()),
            on_drag,
        }
    }
}
