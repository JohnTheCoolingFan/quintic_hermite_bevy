#![allow(
    clippy::type_complexity,
    reason = "bevy systems frequently use complex types"
)]

use std::{f32::consts::PI, ops::Neg};

use bevy::{
    color::palettes::basic::*, math::VectorSpace, prelude::*, sprite::MaterialMesh2dBundle,
};
use bevy_inspector_egui::{
    prelude::*,
    quick::{ResourceInspectorPlugin, WorldInspectorPlugin},
};
use bevy_mod_picking::{events::DragStart, prelude::*, PickableBundle};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                present_mode: bevy::window::PresentMode::AutoNoVsync,
                canvas: Some("#bevy".to_owned()),
                fit_canvas_to_parent: true,
                prevent_default_event_handling: false,
                ..default()
            }),
            ..default()
        }))
        .add_plugins(WorldInspectorPlugin::new())
        .add_plugins(DefaultPickingPlugins)
        .insert_resource(DebugPickingMode::Normal)
        .insert_resource(ClearColor(BLACK.into()))
        .init_resource::<CameraZoom>()
        .add_plugins(ResourceInspectorPlugin::<CameraZoom>::default())
        .add_systems(Startup, setup)
        .add_systems(
            Update,
            (
                draw_curve,
                camera_scale,
                recalculate_curve.before(draw_curve),
                update_parameters.before(recalculate_curve),
            ),
        )
        .run();
}

#[derive(Debug, Clone, Copy, Component)]
struct PolynomialCurveSegment<P: VectorSpace> {
    coeff: [P; 6],
}

impl<P: VectorSpace> PolynomialCurveSegment<P> {
    fn coefficients(p: [P; 6], char_matrix: [[f32; 6]; 6]) -> Self {
        let [c0, c1, c2, c3, c4, c5] = char_matrix;
        let coeff = [
            p[0] * c0[0] + p[1] * c0[1] + p[2] * c0[2] + p[3] * c0[3] + p[4] * c0[4] + p[5] * c0[5],
            p[0] * c1[0] + p[1] * c1[1] + p[2] * c1[2] + p[3] * c1[3] + p[4] * c1[4] + p[5] * c1[5],
            p[0] * c2[0] + p[1] * c2[1] + p[2] * c2[2] + p[3] * c2[3] + p[4] * c2[4] + p[5] * c2[5],
            p[0] * c3[0] + p[1] * c3[1] + p[2] * c3[2] + p[3] * c3[3] + p[4] * c3[4] + p[5] * c3[5],
            p[0] * c4[0] + p[1] * c4[1] + p[2] * c4[2] + p[3] * c4[3] + p[4] * c4[4] + p[5] * c4[5],
            p[0] * c5[0] + p[1] * c5[1] + p[2] * c5[2] + p[3] * c5[3] + p[4] * c5[4] + p[5] * c5[5],
        ];
        Self { coeff }
    }

    fn position(&self, t: f32) -> P {
        let [a, b, c, d, e, f] = self.coeff;

        a + (b + (c + (d + (e + (f) * t) * t) * t) * t) * t
    }
}

#[derive(Debug, Clone, Component)]
struct QuinticHermite<P: VectorSpace> {
    points: Vec<(P, P, P)>,
}

impl<P: VectorSpace> QuinticHermite<P> {
    const CHAR_MATRIX: [[f32; 6]; 6] = [
        [1.0, 0.0, 0.0, 0.0, 0.0, 0.0],
        [0.0, 1.0, 0.0, 0.0, 0.0, 0.0],
        [0.0, 0.0, 0.5, 0.0, 0.0, 0.0],
        [-10.0, -6.0, -1.5, 10.0, -4.0, 0.5],
        [15.0, 8.0, 1.5, -15.0, 7.0, -1.0],
        [-6.0, -3.0, -0.5, 6.0, -3.0, 0.5],
    ];

    fn new(points: impl IntoIterator<Item = (P, P, P)>) -> Self {
        Self {
            points: Vec::from_iter(points),
        }
    }

    pub fn into_segments(self) -> Option<Vec<PolynomialCurveSegment<P>>> {
        if self.points.len() < 2 {
            None
        } else {
            Some(
                Iterator::map(
                    Iterator::map(self.points.windows(2), |pairs| {
                        let (a, b, c) = pairs[0];
                        let (d, e, f) = pairs[1];
                        [a, b, c, d, e, f]
                    }),
                    |points| PolynomialCurveSegment::coefficients(points, Self::CHAR_MATRIX),
                )
                .collect(),
            )
        }
    }
}

fn update_parameters(
    mut curve: Query<&mut PlaygroundCurve, Without<CurveHandle>>,
    handles: Query<(&GlobalTransform, &CurveHandle), Changed<GlobalTransform>>,
) {
    let mut curve_params = curve.single_mut();
    for (handle_transform, handle_id) in handles.iter() {
        let handle_val = handle_transform.translation().xy();
        match handle_id {
            CurveHandle::Position0 => {
                curve_params.pos0 = handle_val;
            }
            CurveHandle::Position1 => {
                curve_params.pos1 = handle_val;
            }
            CurveHandle::Velocity0 => {
                curve_params.vel0 = (handle_val - curve_params.pos0) / ARROW_SCALE;
            }
            CurveHandle::Velocity1 => {
                curve_params.vel1 = (handle_val - curve_params.pos1) / ARROW_SCALE;
            }
            CurveHandle::Acceleration0 => {
                curve_params.acc0 = (handle_val - curve_params.pos0) / ARROW_SCALE;
            }
            CurveHandle::Acceleration1 => {
                curve_params.acc1 = (handle_val - curve_params.pos1) / ARROW_SCALE;
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Component)]
struct PlaygroundCurve {
    pos0: Vec2,
    vel0: Vec2,
    acc0: Vec2,
    pos1: Vec2,
    vel1: Vec2,
    acc1: Vec2,
}

fn recalculate_curve(
    mut curve: Query<
        (&PlaygroundCurve, &mut PolynomialCurveSegment<Vec2>),
        Or<(Changed<PlaygroundCurve>, Added<PlaygroundCurve>)>,
    >,
) {
    let Ok((curve_params, mut curve_polynomial)) = curve.get_single_mut() else {
        return;
    };
    let PlaygroundCurve {
        pos0,
        vel0,
        acc0,
        pos1,
        vel1,
        acc1,
    } = *curve_params;
    let hermite = QuinticHermite::new([(pos0, vel0, acc0), (pos1, vel1, acc1)]);
    let curve_pol = *hermite.into_segments().unwrap().first().unwrap();
    *curve_polynomial = curve_pol;
}

#[derive(Reflect, InspectorOptions, Debug, Clone, Copy, Resource)]
#[reflect(Resource, InspectorOptions)]
struct CameraZoom(f32);

impl Default for CameraZoom {
    fn default() -> Self {
        Self(128.0)
    }
}

fn camera_scale(mut camera: Query<&mut Transform, With<Camera2d>>, scale: Res<CameraZoom>) {
    if scale.is_changed() {
        camera.single_mut().scale = Vec3::splat(scale.0.recip())
    }
}

const ARROW_SCALE: f32 = 0.25;

fn draw_curve(
    the_curve: Query<(&Transform, &PlaygroundCurve, &PolynomialCurveSegment<Vec2>)>,
    mut gizmos: Gizmos,
) {
    gizmos.grid_2d(Vec2::ZERO, 0.0, UVec2::splat(128), Vec2::ONE, GRAY);
    let (_curve_transform, curve_handles, curve) = the_curve.single();
    let pos0 = curve_handles.pos0;
    let vel0 = curve_handles.vel0;
    let acc0 = curve_handles.acc0;
    let pos1 = curve_handles.pos1;
    let vel1 = curve_handles.vel1;
    let acc1 = curve_handles.acc1;
    gizmos.circle_2d(pos0, 16.0_f32.recip(), WHITE);
    gizmos.circle_2d(pos1, 16.0_f32.recip(), WHITE);
    gizmos.arrow_2d(pos0, pos0 + vel0 * ARROW_SCALE, YELLOW);
    gizmos.arrow_2d(pos1, pos1 + vel1 * ARROW_SCALE, YELLOW);
    gizmos.arrow_2d(pos0, pos0 + acc0 * ARROW_SCALE, BLUE);
    gizmos.arrow_2d(pos1, pos1 + acc1 * ARROW_SCALE, BLUE);

    let positions_iter = (0..128).map(|i| {
        let t = 128.0_f32.recip() * i as f32;
        curve.position(t)
    });
    gizmos.linestrip_2d(positions_iter, WHITE)
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Component)]
enum CurveHandle {
    Position0,
    Velocity0,
    Acceleration0,
    Position1,
    Velocity1,
    Acceleration1,
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    let circle_mesh_handle = meshes.add(Circle::new(16.0_f32.recip()));
    let color_mat_handle = materials.add(ColorMaterial::from_color(WHITE));

    let pos0 = Vec2 { x: -1.0, y: 0.0 };
    let vel0 = Vec2 { x: 0.0, y: PI };
    let acc0 = Vec2 {
        x: PI * 2.0,
        y: 0.0,
    };
    let pos1 = Vec2 { x: 1.0, y: 0.0 };
    let vel1 = Vec2 { x: 0.0, y: -PI };
    let acc1 = Vec2 {
        x: -PI * 2.0,
        y: 0.0,
    };

    commands.spawn(Camera2dBundle::default());
    commands
        .spawn((
            TransformBundle::default(),
            PlaygroundCurve {
                pos0,
                vel0,
                acc0,
                pos1,
                vel1,
                acc1,
            },
            PolynomialCurveSegment {
                coeff: [
                    Vec2::ZERO,
                    Vec2::ZERO,
                    Vec2::ZERO,
                    Vec2::ZERO,
                    Vec2::ZERO,
                    Vec2::ZERO,
                ],
            },
        ))
        .with_children(|cb| {
            cb.spawn((
                CurveHandle::Position0,
                MaterialMesh2dBundle {
                    mesh: circle_mesh_handle.clone().into(),
                    material: color_mat_handle.clone(),
                    transform: Transform::from_translation(pos0.extend(0.0)),
                    ..default()
                },
                PickableBundle::default(),
                On::<Pointer<DragStart>>::target_insert(Pickable::IGNORE),
                On::<Pointer<DragEnd>>::target_insert(Pickable::default()),
                On::<Pointer<Drag>>::target_component_mut::<Transform>(|drag, transform| {
                    transform.translation += Vec2 {
                        y: drag.delta.y.neg(),
                        ..drag.delta
                    }
                    .extend(0.0)
                        / 128.0;
                }),
            ))
            .with_children(|cbp| {
                cbp.spawn((
                    CurveHandle::Velocity0,
                    MaterialMesh2dBundle {
                        mesh: circle_mesh_handle.clone().into(),
                        material: color_mat_handle.clone(),
                        transform: Transform::from_translation((vel0 * ARROW_SCALE).extend(0.0)),
                        ..default()
                    },
                    PickableBundle::default(),
                    On::<Pointer<DragStart>>::target_insert(Pickable::IGNORE),
                    On::<Pointer<DragEnd>>::target_insert(Pickable::default()),
                ));
                cbp.spawn((
                    CurveHandle::Acceleration0,
                    MaterialMesh2dBundle {
                        mesh: circle_mesh_handle.clone().into(),
                        material: color_mat_handle.clone(),
                        transform: Transform::from_translation((acc0 * ARROW_SCALE).extend(0.0)),
                        ..default()
                    },
                    PickableBundle::default(),
                    On::<Pointer<DragStart>>::target_insert(Pickable::IGNORE),
                    On::<Pointer<DragEnd>>::target_insert(Pickable::default()),
                ));
            });
            cb.spawn((
                CurveHandle::Position1,
                MaterialMesh2dBundle {
                    mesh: circle_mesh_handle.clone().into(),
                    material: color_mat_handle.clone(),
                    transform: Transform::from_translation(pos1.extend(0.0)),
                    ..default()
                },
                PickableBundle::default(),
                On::<Pointer<DragStart>>::target_insert(Pickable::IGNORE),
                On::<Pointer<DragEnd>>::target_insert(Pickable::default()),
                On::<Pointer<Drag>>::target_component_mut::<Transform>(|drag, transform| {
                    transform.translation += Vec2 {
                        y: drag.delta.y.neg(),
                        ..drag.delta
                    }
                    .extend(0.0)
                        / 128.0;
                }),
            ))
            .with_children(|cbp| {
                cbp.spawn((
                    CurveHandle::Velocity1,
                    MaterialMesh2dBundle {
                        mesh: circle_mesh_handle.clone().into(),
                        material: color_mat_handle.clone(),
                        transform: Transform::from_translation((vel1 * ARROW_SCALE).extend(0.0)),
                        ..default()
                    },
                    PickableBundle::default(),
                    On::<Pointer<DragStart>>::target_insert(Pickable::IGNORE),
                    On::<Pointer<DragEnd>>::target_insert(Pickable::default()),
                ));
                cbp.spawn((
                    CurveHandle::Acceleration1,
                    MaterialMesh2dBundle {
                        mesh: circle_mesh_handle.clone().into(),
                        material: color_mat_handle.clone(),
                        transform: Transform::from_translation((acc1 * ARROW_SCALE).extend(0.0)),
                        ..default()
                    },
                    PickableBundle::default(),
                    On::<Pointer<DragStart>>::target_insert(Pickable::IGNORE),
                    On::<Pointer<DragEnd>>::target_insert(Pickable::default()),
                ));
            });
        });
}
