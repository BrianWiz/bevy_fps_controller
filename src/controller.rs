use std::f32::consts::*;

use bevy::{input::mouse::MouseMotion, math::Vec3Swizzles, prelude::*};
use bevy_rapier3d::prelude::*;

#[cfg(feature = "renet")]
use bevy_renet::renet::{transport::NetcodeClientTransport, RenetClient, RenetServer};

/// Manages the FPS controllers. Executes in `PreUpdate`, after bevy's internal
/// input processing is finished.
///
/// If you need a system in `PreUpdate` to execute after FPS Controller's systems,
/// Do it like so:
///
/// ```
/// # use bevy::prelude::*;
///
/// struct MyPlugin;
/// impl Plugin for MyPlugin {
///     fn build(&self, app: &mut App) {
///         app.add_systems(
///             PreUpdate,
///             (my_system).after( bevy_fps_controller::controller::fps_controller_render )
///         );
///     }
/// }
///
/// fn my_system() { }
/// ```
pub struct FpsControllerPlugin;

impl Plugin for FpsControllerPlugin {
    fn build(&self, app: &mut App) {
        use bevy::input::{gamepad, keyboard, mouse, touch};

        app.add_systems(
            PreUpdate,
            (
                fps_controller_input,
                fps_controller_move_locally_owned,
                #[cfg(feature = "renet")]
                fps_controller_server_move_others.run_if(resource_exists::<RenetServer>()),
                fps_controller_render_locally_owned,
            )
                .chain()
                .after(mouse::mouse_button_input_system)
                .after(keyboard::keyboard_input_system)
                .after(gamepad::gamepad_axis_event_system)
                .after(gamepad::gamepad_button_event_system)
                .after(gamepad::gamepad_connection_system)
                .after(gamepad::gamepad_event_system)
                .after(touch::touch_screen_input_system),
        )
        .insert_resource(FpsControllerConfig {
            always_run: true,
            ..default()
        })
        .insert_resource(FpsControllerPlayerId::default())
        .insert_resource(FpsControllerInputState::default())
        .add_event::<FpsControllerInput>();
    }
}

#[derive(PartialEq)]
pub enum MoveMode {
    Noclip,
    Ground,
}

#[derive(Component)]
pub struct LocallyOwned;

#[derive(Component)]
pub struct LogicalPlayer;

#[derive(Component)]
pub struct RenderPlayer {
    pub logical_entity: Entity,
}

#[derive(Component)]
pub struct CameraConfig {
    pub height_offset: f32,
    pub radius_scale: f32,
}

#[derive(Resource, Component, Default)]
pub struct FpsControllerPlayerId {
    pub id: u64,
}

#[derive(Resource, Default)]
pub struct FpsControllerInputState {
    pub pitch: f32,
    pub yaw: f32,
    pub next_input_id: u64,
}

#[derive(Event, Default, Clone)]
pub struct FpsControllerInput {
    pub owner_id: u64,
    pub input_id: u64,
    pub fly: bool,
    pub sprint: bool,
    pub jump: bool,
    pub crouch: bool,
    pub pitch: f32,
    pub yaw: f32,
    pub movement: Vec3,
}

#[derive(Resource)]
pub struct FpsControllerConfig {
    pub radius: f32,
    pub always_run: bool,
    pub walk_speed: f32,
    pub run_speed: f32,
    pub forward_speed: f32,
    pub side_speed: f32,
    pub air_speed_cap: f32,
    pub air_acceleration: f32,
    pub max_air_speed: f32,
    pub acceleration: f32,
    pub friction: f32,
    /// If the dot product (alignment) of the normal of the surface and the upward vector,
    /// which is a value from [-1, 1], is greater than this value, ground movement is applied
    pub traction_normal_cutoff: f32,
    pub friction_speed_cutoff: f32,
    pub jump_speed: f32,
    pub fly_speed: f32,
    pub crouched_speed: f32,
    pub crouch_speed: f32,
    pub uncrouch_speed: f32,
    pub upright_height: f32,
    pub crouch_height: f32,
    pub fast_fly_speed: f32,
    pub fly_friction: f32,
    pub stop_speed: f32,
    pub sensitivity: f32,
    pub enable_input: bool,
    pub step_offset: f32,
    pub key_forward: KeyCode,
    pub key_back: KeyCode,
    pub key_left: KeyCode,
    pub key_right: KeyCode,
    pub key_up: KeyCode,
    pub key_down: KeyCode,
    pub key_sprint: KeyCode,
    pub key_jump: KeyCode,
    pub key_fly: KeyCode,
    pub key_crouch: KeyCode,
}

impl Default for FpsControllerConfig {
    fn default() -> Self {
        Self {
            radius: 0.5,
            fly_speed: 10.0,
            fast_fly_speed: 30.0,
            always_run: false,
            walk_speed: 9.0,
            run_speed: 14.0,
            forward_speed: 30.0,
            side_speed: 30.0,
            air_speed_cap: 2.0,
            air_acceleration: 20.0,
            max_air_speed: 15.0,
            crouched_speed: 5.0,
            crouch_speed: 6.0,
            uncrouch_speed: 8.0,
            upright_height: 2.0,
            crouch_height: 1.25,
            acceleration: 10.0,
            friction: 10.0,
            traction_normal_cutoff: 0.7,
            friction_speed_cutoff: 0.1,
            fly_friction: 0.5,
            stop_speed: 1.0,
            jump_speed: 8.5,
            step_offset: 0.0,
            enable_input: true,
            key_forward: KeyCode::W,
            key_back: KeyCode::S,
            key_left: KeyCode::A,
            key_right: KeyCode::D,
            key_up: KeyCode::Q,
            key_down: KeyCode::E,
            key_sprint: KeyCode::ShiftLeft,
            key_jump: KeyCode::Space,
            key_fly: KeyCode::F,
            key_crouch: KeyCode::ControlLeft,
            sensitivity: 0.001,
        }
    }
}

#[derive(Component)]
pub struct FpsControllerState {
    pub move_mode: MoveMode,
    pub gravity: f32,
    pub pitch: f32,
    pub yaw: f32,
    pub ground_tick: u8,
    pub grounded: bool,
    pub height: f32,
    pub last_processed_input: FpsControllerInput,
}

impl Default for FpsControllerState {
    fn default() -> Self {
        Self {
            move_mode: MoveMode::Ground,
            gravity: 23.0,
            pitch: 0.0,
            yaw: 0.0,
            ground_tick: 0,
            grounded: false,
            height: 1.5,
            last_processed_input: FpsControllerInput::default(),
        }
    }
}

// ██╗      ██████╗  ██████╗ ██╗ ██████╗
// ██║     ██╔═══██╗██╔════╝ ██║██╔════╝
// ██║     ██║   ██║██║  ███╗██║██║
// ██║     ██║   ██║██║   ██║██║██║
// ███████╗╚██████╔╝╚██████╔╝██║╚██████╗
// ╚══════╝ ╚═════╝  ╚═════╝ ╚═╝ ╚═════╝

const ANGLE_EPSILON: f32 = 0.001953125;

pub fn fps_controller_input(
    key_input: Res<Input<KeyCode>>,
    config: Res<FpsControllerConfig>,
    player_id: Res<FpsControllerPlayerId>,
    mut input_state: ResMut<FpsControllerInputState>,
    mut mouse_events: EventReader<MouseMotion>,
    mut input_writer: EventWriter<FpsControllerInput>,
) {
    if !config.enable_input {
        return;
    }

    let mut new_input = FpsControllerInput {
        owner_id: player_id.id,
        input_id: input_state.next_input_id,
        fly: false,
        sprint: false,
        jump: false,
        crouch: false,
        pitch: input_state.pitch,
        yaw: input_state.yaw,
        movement: Vec3::ZERO,
    };

    input_state.next_input_id += 1;

    let mut mouse_delta = Vec2::ZERO;
    for mouse_event in mouse_events.read() {
        mouse_delta += mouse_event.delta;
    }
    mouse_delta *= config.sensitivity;

    input_state.pitch = (input_state.pitch - mouse_delta.y)
        .clamp(-FRAC_PI_2 + ANGLE_EPSILON, FRAC_PI_2 - ANGLE_EPSILON);
    input_state.yaw -= mouse_delta.x;
    if input_state.yaw.abs() > PI {
        input_state.yaw = input_state.yaw.rem_euclid(TAU);
    }

    new_input.movement = Vec3::new(
        get_axis(&key_input, config.key_right, config.key_left),
        get_axis(&key_input, config.key_up, config.key_down),
        get_axis(&key_input, config.key_forward, config.key_back),
    );

    if config.always_run {
        new_input.sprint = !key_input.pressed(config.key_sprint);
    } else {
        new_input.sprint = key_input.pressed(config.key_sprint);
    }

    new_input.jump = key_input.pressed(config.key_jump);
    new_input.fly = key_input.just_pressed(config.key_fly);
    new_input.crouch = key_input.pressed(config.key_crouch);
    new_input.yaw = input_state.yaw;
    new_input.pitch = input_state.pitch;

    input_writer.send(new_input);
}

pub fn fps_controller_move_locally_owned(
    time: Res<Time>,
    physics_context: Res<RapierContext>,
    config: Res<FpsControllerConfig>,
    local_player_id: Res<FpsControllerPlayerId>,
    mut query: Query<
        (
            Entity,
            &mut FpsControllerState,
            &mut Collider,
            &mut Transform,
            &mut Velocity,
        ),
        With<LocallyOwned>,
    >,
    mut input_reader: EventReader<FpsControllerInput>,
) {
    let dt = time.delta_seconds();

    // loop through events from input_reader
    for input in input_reader.read() {
        if input.owner_id != local_player_id.id {
            continue;
        }
        for (entity, mut controller, mut collider, mut transform, mut velocity) in query.iter_mut()
        {
            move_controller(
                dt,
                &entity,
                &physics_context,
                &config,
                &input,
                &mut transform,
                &mut velocity,
                &mut collider,
                &mut controller,
            );
        }
    }
}

pub fn fps_controller_server_move_others(
    time: Res<Time>,
    physics_context: Res<RapierContext>,
    config: Res<FpsControllerConfig>,
    mut query: Query<
        (
            Entity,
            &mut FpsControllerState,
            &mut Collider,
            &mut Transform,
            &mut Velocity,
            &FpsControllerPlayerId,
        ),
        Without<LocallyOwned>,
    >,
    mut input_reader: EventReader<FpsControllerInput>,
) {
    let dt = time.delta_seconds();

    // loop through events from input_reader
    for (entity, mut controller, mut collider, mut transform, mut velocity, owner_id) in
        query.iter_mut()
    {
        let mut inputs = Vec::new();
        for input in input_reader.read() {
            if input.owner_id == owner_id.id {
                inputs.push(input.clone());
            }
        }

        // if there are no inputs for this controller, we create an input based on their
        // last known state this can happen during packet loss,or if the client is ticking
        // slower than the server, or if they're trying to cheat by not sending inputs
        // (ex cheat: trying to stop all momentum mid-air)
        if inputs.len() == 0 {
            let input = FpsControllerInput {
                owner_id: owner_id.id,
                input_id: controller.last_processed_input.input_id,
                fly: controller.last_processed_input.fly,
                sprint: controller.last_processed_input.sprint,
                jump: controller.last_processed_input.jump,
                crouch: controller.last_processed_input.crouch,
                pitch: controller.last_processed_input.pitch,
                yaw: controller.last_processed_input.yaw,
                movement: controller.last_processed_input.movement,
            };
            move_controller(
                dt,
                &entity,
                &physics_context,
                &config,
                &input,
                &mut transform,
                &mut velocity,
                &mut collider,
                &mut controller,
            );
        } else {
            for input in inputs {
                move_controller(
                    dt,
                    &entity,
                    &physics_context,
                    &config,
                    &input,
                    &mut transform,
                    &mut velocity,
                    &mut collider,
                    &mut controller,
                );
            }
        }
    }
}

pub fn move_controller(
    delta_seconds: f32,
    entity: &Entity,
    physics_context: &Res<RapierContext>,
    config: &FpsControllerConfig,
    input: &FpsControllerInput,
    transform: &mut Transform,
    velocity: &mut Velocity,
    collider: &mut Collider,
    controller: &mut FpsControllerState,
) {
    let dt = delta_seconds;

    controller.last_processed_input = input.clone();
    controller.pitch = input.pitch;
    controller.yaw = input.yaw;

    if input.fly {
        controller.move_mode = match controller.move_mode {
            MoveMode::Noclip => MoveMode::Ground,
            MoveMode::Ground => MoveMode::Noclip,
        }
    }

    match controller.move_mode {
        MoveMode::Noclip => {
            if input.movement == Vec3::ZERO {
                let friction = config.fly_friction.clamp(0.0, 1.0);
                velocity.linvel *= 1.0 - friction;
                if velocity.linvel.length_squared() < f32::EPSILON {
                    velocity.linvel = Vec3::ZERO;
                }
            } else {
                let fly_speed = if input.sprint {
                    config.fast_fly_speed
                } else {
                    config.fly_speed
                };
                let mut move_to_world =
                    Mat3::from_euler(EulerRot::YXZ, input.yaw, input.pitch, 0.0);
                move_to_world.z_axis *= -1.0; // Forward is -Z
                move_to_world.y_axis = Vec3::Y; // Vertical movement aligned with world up
                velocity.linvel = move_to_world * input.movement * fly_speed;
            }
        }
        MoveMode::Ground => {
            if let Some(capsule) = collider.as_capsule() {
                // Capsule cast downwards to find ground
                // Better than a ray cast as it handles when you are near the edge of a surface
                let capsule = capsule.raw;
                let cast_capsule = Collider::capsule(
                    capsule.segment.a.into(),
                    capsule.segment.b.into(),
                    capsule.radius * 0.9,
                );
                // Avoid self collisions
                let filter = QueryFilter::default().exclude_rigid_body(*entity);
                let ground_cast = physics_context.cast_shape(
                    transform.translation,
                    transform.rotation,
                    -Vec3::Y,
                    &cast_capsule,
                    0.125,
                    true,
                    filter,
                );

                let speeds = Vec3::new(config.side_speed, 0.0, config.forward_speed);
                let mut move_to_world = Mat3::from_axis_angle(Vec3::Y, input.yaw);
                move_to_world.z_axis *= -1.0; // Forward is -Z
                let mut wish_direction = move_to_world * (input.movement * speeds);
                let mut wish_speed = wish_direction.length();
                if wish_speed > f32::EPSILON {
                    // Avoid division by zero
                    wish_direction /= wish_speed; // Effectively normalize, avoid length computation twice
                }
                let max_speed = if input.crouch {
                    config.crouched_speed
                } else if input.sprint {
                    config.run_speed
                } else {
                    config.walk_speed
                };
                wish_speed = f32::min(wish_speed, max_speed);

                // grounded
                if let Some((toi, toi_details)) = toi_details_unwrap(ground_cast) {
                    controller.grounded = true;
                    let has_traction =
                        Vec3::dot(toi_details.normal1, Vec3::Y) > config.traction_normal_cutoff;

                    // Only apply friction after at least one tick, allows b-hopping without losing speed
                    if controller.ground_tick >= 1 && has_traction {
                        let lateral_speed = velocity.linvel.xz().length();
                        if lateral_speed > config.friction_speed_cutoff {
                            let control = f32::max(lateral_speed, config.stop_speed);
                            let drop = control * config.friction * dt;
                            let new_speed = f32::max((lateral_speed - drop) / lateral_speed, 0.0);
                            velocity.linvel.x *= new_speed;
                            velocity.linvel.z *= new_speed;
                        } else {
                            velocity.linvel = Vec3::ZERO;
                        }
                        if controller.ground_tick == 1 {
                            velocity.linvel.y = -toi.toi;
                        }
                    }

                    let mut add = acceleration(
                        wish_direction,
                        wish_speed,
                        config.acceleration,
                        velocity.linvel,
                        dt,
                    );
                    if !has_traction {
                        add.y -= controller.gravity * dt;
                    }
                    velocity.linvel += add;

                    if has_traction {
                        let linvel = velocity.linvel;
                        velocity.linvel -=
                            Vec3::dot(linvel, toi_details.normal1) * toi_details.normal1;

                        if input.jump {
                            velocity.linvel.y = config.jump_speed;
                        }
                    }

                    // Increment ground tick but cap at max value
                    controller.ground_tick = controller.ground_tick.saturating_add(1);
                // in air
                } else {
                    controller.grounded = false;
                    controller.ground_tick = 0;

                    wish_speed = f32::min(wish_speed, config.air_speed_cap);

                    let mut add = acceleration(
                        wish_direction,
                        wish_speed,
                        config.air_acceleration,
                        velocity.linvel,
                        dt,
                    );
                    add.y = -controller.gravity * dt;
                    velocity.linvel += add;

                    let air_speed = velocity.linvel.xz().length();
                    if air_speed > config.max_air_speed {
                        let ratio = config.max_air_speed / air_speed;
                        velocity.linvel.x *= ratio;
                        velocity.linvel.z *= ratio;
                    }
                }

                /* Crouching */

                let crouch_height = config.crouch_height;
                let upright_height = config.upright_height;

                let crouch_speed = if input.crouch {
                    -config.crouch_speed
                } else {
                    config.uncrouch_speed
                };
                controller.height += dt * crouch_speed;
                controller.height = controller.height.clamp(crouch_height, upright_height);

                if let Some(mut capsule) = collider.as_capsule_mut() {
                    capsule.set_segment(Vec3::Y * 0.5, Vec3::Y * controller.height);
                }

                // Step offset
                if config.step_offset > f32::EPSILON && controller.ground_tick >= 1 {
                    let cast_offset = velocity.linvel.normalize_or_zero() * config.radius * 1.0625;
                    let cast = physics_context.cast_ray_and_get_normal(
                        transform.translation + cast_offset + Vec3::Y * config.step_offset * 1.0625,
                        -Vec3::Y,
                        config.step_offset * 0.9375,
                        false,
                        filter,
                    );
                    if let Some((_, hit)) = cast {
                        transform.translation.y += config.step_offset * 1.0625 - hit.toi;
                        transform.translation += cast_offset;
                    }
                }

                // Prevent falling off ledges
                if controller.ground_tick >= 1 && input.crouch {
                    for _ in 0..2 {
                        // Find the component of our velocity that is overhanging and subtract it off
                        let overhang = overhang_component(
                            *entity,
                            transform,
                            physics_context.as_ref(),
                            velocity.linvel,
                            dt,
                        );
                        if let Some(overhang) = overhang {
                            velocity.linvel -= overhang;
                        }
                    }
                    // If we are still overhanging consider unsolvable and freeze
                    if overhang_component(
                        *entity,
                        transform,
                        physics_context.as_ref(),
                        velocity.linvel,
                        dt,
                    )
                    .is_some()
                    {
                        velocity.linvel = Vec3::ZERO;
                    }
                }
            }
        }
    }
}

fn toi_details_unwrap(ground_cast: Option<(Entity, Toi)>) -> Option<(Toi, ToiDetails)> {
    if let Some((_, toi)) = ground_cast {
        if let Some(details) = toi.details {
            return Some((toi, details));
        }
    }
    None
}

fn overhang_component(
    entity: Entity,
    transform: &Transform,
    physics_context: &RapierContext,
    velocity: Vec3,
    dt: f32,
) -> Option<Vec3> {
    // Cast a segment (zero radius on capsule) from our next position back towards us
    // If there is a ledge in front of us we will hit the edge of it
    // We can use the normal of the hit to subtract off the component that is overhanging
    let cast_capsule = Collider::capsule(Vec3::Y * 0.125, -Vec3::Y * 0.125, 0.0);
    let filter = QueryFilter::default().exclude_rigid_body(entity);
    let future_position = transform.translation + velocity * dt;
    let cast = physics_context.cast_shape(
        future_position,
        transform.rotation,
        -velocity,
        &cast_capsule,
        0.5,
        true,
        filter,
    );
    if let Some((_, toi_details)) = toi_details_unwrap(cast) {
        let cast = physics_context.cast_ray(
            future_position + Vec3::Y * 0.125,
            -Vec3::Y,
            0.375,
            false,
            filter,
        );
        // Make sure that this is actually a ledge, e.g. there is no ground in front of us
        if cast.is_none() {
            let normal = -toi_details.normal1;
            let alignment = Vec3::dot(velocity, normal);
            return Some(alignment * normal);
        }
    }
    None
}

fn acceleration(
    wish_direction: Vec3,
    wish_speed: f32,
    acceleration: f32,
    velocity: Vec3,
    dt: f32,
) -> Vec3 {
    let velocity_projection = Vec3::dot(velocity, wish_direction);
    let add_speed = wish_speed - velocity_projection;
    if add_speed <= 0.0 {
        return Vec3::ZERO;
    }

    let acceleration_speed = f32::min(acceleration * wish_speed * dt, add_speed);
    wish_direction * acceleration_speed
}

fn get_pressed(key_input: &Res<Input<KeyCode>>, key: KeyCode) -> f32 {
    if key_input.pressed(key) {
        1.0
    } else {
        0.0
    }
}

fn get_axis(key_input: &Res<Input<KeyCode>>, key_pos: KeyCode, key_neg: KeyCode) -> f32 {
    get_pressed(key_input, key_pos) - get_pressed(key_input, key_neg)
}

// ██████╗ ███████╗███╗   ██╗██████╗ ███████╗██████╗
// ██╔══██╗██╔════╝████╗  ██║██╔══██╗██╔════╝██╔══██╗
// ██████╔╝█████╗  ██╔██╗ ██║██║  ██║█████╗  ██████╔╝
// ██╔══██╗██╔══╝  ██║╚██╗██║██║  ██║██╔══╝  ██╔══██╗
// ██║  ██║███████╗██║ ╚████║██████╔╝███████╗██║  ██║
// ╚═╝  ╚═╝╚══════╝╚═╝  ╚═══╝╚═════╝ ╚══════╝╚═╝  ╚═╝

pub fn fps_controller_render_locally_owned(
    mut render_query: Query<
        (&mut Transform, &RenderPlayer),
        (With<RenderPlayer>, With<LocallyOwned>),
    >,
    logical_query: Query<
        (&Transform, &Collider, &FpsControllerState, &CameraConfig),
        (
            With<LogicalPlayer>,
            With<LocallyOwned>,
            Without<RenderPlayer>,
        ),
    >,
) {
    for (mut render_transform, render_player) in render_query.iter_mut() {
        if let Ok((logical_transform, collider, controller, camera_config)) =
            logical_query.get(render_player.logical_entity)
        {
            if let Some(capsule) = collider.as_capsule() {
                let camera_height = capsule.segment().b().y
                    + capsule.radius() * camera_config.radius_scale
                    + camera_config.height_offset;
                render_transform.translation =
                    logical_transform.translation + Vec3::Y * camera_height;
                render_transform.rotation =
                    Quat::from_euler(EulerRot::YXZ, controller.yaw, controller.pitch, 0.0);
            }
        }
    }
}
