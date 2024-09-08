#![allow(
    clippy::type_complexity,
    reason = "bevy systems frequently use complex types"
)]

mod arrow_handle;
mod curve_tools;
mod quintic_hermite;
use std::f32::consts::PI;

use arrow_handle::*;
use bevy::{color::palettes::basic::*, math::VectorSpace, prelude::*, sprite::Anchor};
use bevy_inspector_egui::{
    prelude::*,
    quick::{ResourceInspectorPlugin, WorldInspectorPlugin},
};
use bevy_mod_picking::prelude::*;
use quintic_hermite::*;

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
    let PlaygroundCurve {
        pos0,
        vel0,
        acc0,
        pos1,
        vel1,
        acc1,
    } = *curve_handles;
    gizmos.circle_2d(pos0, 16.0_f32.recip(), WHITE);
    gizmos.circle_2d(pos1, 16.0_f32.recip(), WHITE);
    gizmos.arrow_2d(pos0, pos0 + vel0 * ARROW_SCALE, YELLOW);
    gizmos.arrow_2d(pos1, pos1 + vel1 * ARROW_SCALE, YELLOW);
    gizmos.arrow_2d(pos0, pos0 + acc0 * ARROW_SCALE, BLUE);
    gizmos.arrow_2d(pos1, pos1 + acc1 * ARROW_SCALE, BLUE);

    let mut curve_info_text = info_text.single_mut();

    if draw_options.draw_parametric_length_range {
        let (min, max, diff) = curve.segment_min_max_diff(128);

        curve_info_text.sections = vec![TextSection::from(format!(
            "Curve parametric length info: max {max:.6}; min {min:.6}; difference {diff:.6}"
        ))];

        for (start, end) in curve.segments_iter(128) {
            let segment_length = (end - start).length();
            let lerp_val = (segment_length - min) / diff;
            let segment_color = GREEN.mix(&RED, lerp_val);
            gizmos.line_2d(start, end, segment_color);
        }
    } else {
        curve_info_text.sections.clear();
        gizmos.linestrip_2d(curve.positions_iter(128), WHITE)
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

    let handle_factory = HandleBundleFactory::setup(&mut meshes, &mut materials);

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
            PolynomialCurveSegment::<Vec2>::default(),
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
