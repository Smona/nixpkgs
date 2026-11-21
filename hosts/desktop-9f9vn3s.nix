{ self, ... }:
{
  den.aspects."smona@DESKTOP-9F9VN3S".homeManager = {
    imports = [
      self.homeModules.linux-pc
    ];
  };
}
