with Clips;

procedure Hello_World is
begin
   Clips.Initialize;
   Clips.Load("hello_world.clp");
   Clips.Reset;
   Clips.Run;
end Hello_World;
