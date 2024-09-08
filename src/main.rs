#![allow(
    clippy::type_complexity,
    reason = "bevy systems frequently use complex types"
)]

use std::{f32::consts::PI, ops::Neg};

use bevy::{
    color::palettes::basic::*,
    math::VectorSpace,
    prelude::*,
    sprite::{Anchor, Material2d, MaterialMesh2dBundle, Mesh2dHandle},
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
        .init_resource::<DrawOptions>()
        .add_plugins(ResourceInspectorPlugin::<CameraZoom>::default())
        .add_plugins(ResourceInspectorPlugin::<DrawOptions>::default())
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

#[derive(Reflect, InspectorOptions, Debug, Clone, Copy, Resource, Default)]
#[reflect(Resource, InspectorOptions)]
struct DrawOptions {
    draw_parametric_length_range: bool,
}

fn draw_curve(
    the_curve: Query<
        (&Transform, &PlaygroundCurve, &PolynomialCurveSegment<Vec2>),
        Without<CurveInfoTextMarker>,
    >,
    mut info_text: Query<&mut Text, With<CurveInfoTextMarker>>,
    draw_options: Res<DrawOptions>,
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

    let mut curve_info_text = info_text.single_mut();

    if draw_options.draw_parametric_length_range {
        let positions: Vec<Vec2> = (0..=128)
            .map(|i| {
                let t = 128.0_f32.recip() * i as f32;
                curve.position(t)
            })
            .collect();
        let lengths = positions
            .windows(2)
            .map(|win| {
                let [start, end, ..] = win else {
                    unreachable!()
                };
                (*end - *start).length()
            })
            .collect::<Vec<_>>();
        let mut max = f32::ZERO;
        let mut min = f32::INFINITY;
        for line_length in lengths {
            if line_length > max {
                max = line_length
            }
            if line_length < min {
                min = line_length
            }
        }

        curve_info_text.sections = vec![TextSection::from(format!(
            "Curve parametric length info: max {max:.6}; min {min:.6}; difference {:.6}",
            max - min
        ))];

        for (start, end) in positions.windows(2).map(|segment| {
            let [start, end, ..] = *segment else {
                unreachable!()
            };
            (start, end)
        }) {
            let segment_length = (end - start).length();
            let lerp_val = (segment_length - min) / (max - min);
            let segment_color = GREEN.mix(&RED, lerp_val);
            gizmos.line_2d(start, end, segment_color);
        }
    } else {
        curve_info_text.sections.clear();
        let positions_iter = (0..=128).map(|i| {
            let t = 128.0_f32.recip() * i as f32;
            curve.position(t)
        });
        gizmos.linestrip_2d(positions_iter, WHITE)
    }
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

struct HandleBundleFactory<M: Material2d> {
    shape_handle: Mesh2dHandle,
    shape_material: Handle<M>,
}

impl<M: Material2d> HandleBundleFactory<M> {
    fn new(shape_handle: Mesh2dHandle, shape_material: Handle<M>) -> Self {
        Self {
            shape_handle,
            shape_material,
        }
    }

    fn new_handle(&self, pos: Vec2) -> HandleBundle<M> {
        HandleBundle::new(
            pos,
            &self.shape_handle,
            &self.shape_material,
            On::<Pointer<Drag>>::target_component_mut::<Transform>(|drag, transform| {
                transform.translation += Vec2 {
                    y: drag.delta.y.neg(),
                    ..drag.delta
                }
                .extend(0.0)
                    / 128.0;
            }),
        )
    }

    fn new_child_handle(&self, pos: Vec2) -> HandleBundle<M> {
        HandleBundle::new(
            pos,
            &self.shape_handle,
            &self.shape_material,
            On::<Pointer<Drag>>::run(|_: ()| {}),
        )
    }
}

#[derive(Bundle)]
struct HandleBundle<M: Material2d> {
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

#[derive(Debug, Clone, Copy, Component)]
struct CurveInfoTextMarker;

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    commands.spawn(Text2dBundle {
        text: Text::from_sections([
            TextSection::from("Drag white circles to change input values\n"),
            TextSection::from("Yellow arrows are velocity (first derivative)\n"),
            TextSection::from("Blue arrows are acceleration (second derivative)\n"),
            TextSection::from(
                "Red segments have bigger length than green, on the scale of the whole curve\n",
            ),
        ]),
        transform: Transform::from_xyz(-4.5, -1.0, 0.0).with_scale(Vec3::splat(128.0_f32.recip())),
        text_anchor: Anchor::TopLeft,
        ..default()
    });
    commands.spawn((
        CurveInfoTextMarker,
        Text2dBundle {
            text: Text::from_sections([]),
            text_anchor: Anchor::TopLeft,
            transform: Transform::from_xyz(-4.5, -2.0, 0.0)
                .with_scale(Vec3::splat(128.0_f32.recip())),
            ..default()
        },
    ));

    let circle_mesh_handle = meshes.add(Circle::new(16.0_f32.recip())).into();
    let color_mat_handle = materials.add(ColorMaterial::from_color(WHITE));

    let handle_factory = HandleBundleFactory::new(circle_mesh_handle, color_mat_handle);

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
                Name::new("Position 0"),
                handle_factory.new_handle(pos0),
            ))
            .with_children(|cbp| {
                cbp.spawn((
                    CurveHandle::Velocity0,
                    Name::new("Velocity 0"),
                    handle_factory.new_child_handle(vel0 * ARROW_SCALE),
                ));
                cbp.spawn((
                    CurveHandle::Acceleration0,
                    Name::new("Acceleration 0"),
                    handle_factory.new_child_handle(acc0 * ARROW_SCALE),
                ));
            });
            cb.spawn((
                CurveHandle::Position1,
                Name::new("Position 1"),
                handle_factory.new_handle(pos1),
            ))
            .with_children(|cbp| {
                cbp.spawn((
                    CurveHandle::Velocity1,
                    Name::new("Velocity 1"),
                    handle_factory.new_child_handle(vel1 * ARROW_SCALE),
                ));
                cbp.spawn((
                    CurveHandle::Acceleration1,
                    Name::new("Acceleration 1"),
                    handle_factory.new_child_handle(acc1 * ARROW_SCALE),
                ));
            });
        });
}
