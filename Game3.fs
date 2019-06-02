namespace MonoGame003

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open System.Collections.Generic

type Sprite =
    {position: Vector2; speed: float32; texture: Texture2D; size: Point; offset: Point}
    member this.Draw(spriteBatch: SpriteBatch) =
        let sourceRect = Rectangle(this.offset, this.size)
        spriteBatch.Draw(this.texture, this.position, Nullable.op_Implicit sourceRect, Color.White)

module AnimationFrames =
    let horizontalStrip(frameCount, size: Point, offset: Point) =
        [| for i in 0..frameCount-1 ->
            Rectangle(offset + Point(size.X * i, 0) , size)
        |]

type Animation = 
    {   frames: Rectangle array
        fps: int
        currentFrame: int
        frameTimer: TimeSpan
        frameLength: TimeSpan
        size: Point }

    static member Create(frameCount, fps, size: Point, offset: Point) =
        let frames = AnimationFrames.horizontalStrip(frameCount, size, offset)
        {   frames = frames
            currentFrame = 0
            frameTimer = TimeSpan.Zero
            frameLength = TimeSpan.FromSeconds (float (1.f / float32 fps))
            fps = fps
            size = size }
    
    member this.CurrentFrame =
        this.frames.[this.currentFrame]

module Animation =
    let reset anim =
        {anim with
            currentFrame = 0
            frameTimer = TimeSpan.Zero }
        
    let update (gameTime: GameTime) (anim: Animation) =
        let newframeTimer, newFrame =
            match anim.frameTimer + gameTime.ElapsedGameTime with
            | n when n >= anim.frameLength ->
                TimeSpan.Zero, (anim.currentFrame + 1) % anim.frames.Length
            | n -> n, anim.currentFrame

        {anim with
            frameTimer = newframeTimer
            currentFrame = newFrame }

type AnimationKey =
    | IdleLeft
    | IdleRight
    | IdleDown
    | IdleUp
    | WalkLeft
    | WalkRight
    | WalkDown
    | WalkUp

type AnimatedSprite =
    {   texture: Texture2D
        animations: Map<AnimationKey, Animation>
        currentAnimationKey: AnimationKey
        isAnimating: bool
        speed: float32
        position: Vector2 }
    member this.CurrentAnimation = this.animations.[this.currentAnimationKey]
    member this.Size with get() = this.CurrentAnimation.size

module AnimatedSprite =  

    let resetAnimation key animatedSprite =
        animatedSprite.animations.[key]
        |> Animation.reset
        
    let updateAnimation key gameTime animatedSprite =
        let animation = animatedSprite.animations.[key]
        if animatedSprite.isAnimating then
            animation |> Animation.update gameTime
        else animation
        
    let draw (animSprite: AnimatedSprite) (gameTime: GameTime) (sb: SpriteBatch) =
        sb.Draw(animSprite.texture, animSprite.position, Nullable.op_Implicit animSprite.CurrentAnimation.CurrentFrame, Color.White)

[<AutoOpen>]
module MonoGameExtensions =
    type Viewport with
        member this.Center =
            Vector2(float32 this.Width * 0.5f, float32 this.Height * 0.5f)

type Camera(viewport: Viewport) =       
    member val WorldToScreen = Matrix.Identity with get, set
    member val ScreenToWorld = Matrix.Identity with get, set
    member val Zoom = 1.0f with get, set
    member val Position = Vector2.Zero with get, set
    member val Rotation = 0.0f with get, set

    member this.Update (pos:Vector2) =
        this.Position <- pos
        this.WorldToScreen <-
            Matrix.CreateTranslation(Vector3(-pos, 0.0f)) *
            Matrix.CreateRotationZ(this.Rotation ) *
            Matrix.CreateScale(Vector3(this.Zoom, this.Zoom, 1.f )) *
            Matrix.CreateTranslation(Vector3(viewport.Center, 0.f))
        this.ScreenToWorld <- Matrix.Invert(this.WorldToScreen)

type Game3 () as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this, PreferredBackBufferWidth = 1920, PreferredBackBufferHeight = 1080)
    let mutable spriteBatch = Unchecked.defaultof<_>
    let mutable playerSpriteSheet = Unchecked.defaultof<Texture2D>
    let mutable player = Unchecked.defaultof<Sprite>
    let mutable newPlayer = Unchecked.defaultof<AnimatedSprite>
    let mutable playerAnimations = Unchecked.defaultof<_>
    let mutable camera = Unchecked.defaultof<_>

    let (|KeyDown|_|) k (state: KeyboardState) =
        if state.IsKeyDown k then Some() else None

    let getMovementVector = function
        | KeyDown Keys.W & KeyDown Keys.A -> Vector2(-1.f, -1.f), WalkLeft
        | KeyDown Keys.W & KeyDown Keys.D -> Vector2(1.f, -1.f), WalkRight
        | KeyDown Keys.S & KeyDown Keys.A -> Vector2(-1.f, 1.f), WalkLeft
        | KeyDown Keys.S & KeyDown Keys.D -> Vector2(1.f, 1.f), WalkRight
        | KeyDown Keys.W -> Vector2(0.f, -1.f), WalkUp
        | KeyDown Keys.S -> Vector2(0.f, 1.f), WalkDown
        | KeyDown Keys.A -> Vector2(-1.f, 0.f), WalkLeft
        | KeyDown Keys.D -> Vector2(1.f, 0.f), WalkRight
        | _ -> Vector2.Zero, WalkDown

    do
        this.Content.RootDirectory <- "Content"
        this.IsMouseVisible <- false

    override this.Initialize() =
        let frameSize = Point(64, 64)
        let anims = 
            [   IdleUp,    Animation.Create(1, 1, frameSize, Point(0, 0))
                IdleLeft,  Animation.Create(1, 1, frameSize, Point(0, 64))
                IdleDown,  Animation.Create(1, 1, frameSize, Point(0, 128))
                IdleRight, Animation.Create(1, 1, frameSize, Point(0, 192))
                WalkUp,    Animation.Create(8, 10, frameSize, Point(64, 0))
                WalkLeft,  Animation.Create(8, 10, frameSize, Point(64, 64))
                WalkDown,  Animation.Create(8, 10, frameSize, Point(64, 128))
                WalkRight, Animation.Create(8, 10, frameSize, Point(64, 192)) ] |> Map.ofList
        playerAnimations <- anims
        camera <- Camera(this.GraphicsDevice.Viewport)
        base.Initialize()

    override this.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        playerSpriteSheet <- this.Content.Load<Texture2D>("skeleton")
        player <- { position = Vector2.Zero
                    speed= 166.f
                    texture = playerSpriteSheet
                    size = Point(64, 64)
                    offset = Point(0,128) }

        newPlayer <- {  texture = playerSpriteSheet
                        animations = playerAnimations
                        currentAnimationKey = AnimationKey.IdleDown
                        isAnimating = false
                        speed = 166.f
                        position = Vector2.Zero }

    override this.Update (gameTime) =
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
        then this.Exit()

        let walkingToIdle = function
            | WalkUp -> IdleUp
            | WalkLeft -> IdleLeft
            | WalkDown -> IdleDown
            | WalkRight -> IdleRight
            | otherIdle -> otherIdle

        let movementVector, isMoving, animationKey =
            let movementVector, animationKey = getMovementVector(Keyboard.GetState())
            if movementVector = Vector2.Zero then
                movementVector, false, walkingToIdle newPlayer.currentAnimationKey
            else movementVector |> Vector2.Normalize, true, animationKey
            
        let newPosition player =
            let newPos =
                player.position + movementVector * player.speed * float32 gameTime.ElapsedGameTime.TotalSeconds
            
            let playerSize = player.Size.ToVector2()
            
            let minClamp =
                Vector2.Zero - playerSize * 0.5f
            
            let maxClamp =
                Vector2(float32 this.GraphicsDevice.Viewport.Width,
                        float32 this.GraphicsDevice.Viewport.Height) - playerSize * 0.5f
            
            Vector2.Clamp(newPos, minClamp, maxClamp)      

        let newAnimation =
            if newPlayer.currentAnimationKey = animationKey then
                newPlayer |> AnimatedSprite.updateAnimation animationKey gameTime
            else
                newPlayer |> AnimatedSprite.resetAnimation animationKey

        newPlayer <- { newPlayer with
                        position = newPosition newPlayer
                        isAnimating = isMoving
                        currentAnimationKey = animationKey
                        animations = newPlayer.animations |> Map.add animationKey newAnimation }

        camera.Update newPlayer.position

        base.Update(gameTime)

    override this.Draw (gameTime) =
        this.GraphicsDevice.Clear Color.CornflowerBlue
        spriteBatch.Begin(transformMatrix = Nullable.op_Implicit camera.WorldToScreen)
        player.Draw(spriteBatch)
        AnimatedSprite.draw newPlayer gameTime spriteBatch
        spriteBatch.End()
        base.Draw(gameTime)