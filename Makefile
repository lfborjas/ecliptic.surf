serve:
	SE_EPHE_PATH="$(shell pwd)/config/ephe/" EP4_PATH="$(shell pwd)/config/ephe/" DEPLOY_ENV=Development PORT=3333 cabal run
