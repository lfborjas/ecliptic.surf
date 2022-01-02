# ecliptic.surf

Surf charts for planetary transits.

![image](https://user-images.githubusercontent.com/82133/147889255-1347cd1e-2b3d-48be-9ca7-b9510a9593f7.png)

## Development

I use `nix` here. You can enter a `nix-shell` and everything should be set up. Once there, `make serve` will
run the server with the necessary environment variables.

## Deployment

A `docker` derivation is provided -- I build on the cloud with github actions and push to the heroku container
registry.
