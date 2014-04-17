USER = host
SERVER = direct.positionsites.com

all:
	cabal install -fdevelopment && ./.cabal-sandbox/bin/positionsites

run:
	./.cabal-sandbox/bin/positionsites

# deploy: send
# 	ssh $(USER)@$(SERVER) "/var/www/scripts/deploy.sh"


# deploy-static: send-static
# 	ssh $(USER)@$(SERVER) "/var/www/scripts/reload.sh"

# send: send-static
# 	cabal install
# 	scp angel.conf $(USER)@$(SERVER):
# 	scp ./.cabal-sandbox/bin/positionsites $(USER)@$(SERVER):positionsites-new

# send-static:
# 	rsync --checksum -avz -e ssh static/* $(USER)@$(SERVER):static
# 	rsync --checksum -avz -e ssh markdown/* $(USER)@$(SERVER):markdown
# 	rsync --checksum -avz -e ssh snaplets/* $(USER)@$(SERVER):snaplets

# start:
# 	ssh $(USER)@$(SERVER) "/var/www/scripts/start.sh"

migrate:
	rsync --checksum -ave 'ssh '  migrations/* $(USER)@$(SERVER):migrations
	ssh $(USER)@$(SERVER) "/var/www/moo.sh upgrade"

# rollback:
# 	ssh $(USER)@$(SERVER) "/var/www/scripts/rollback.sh"

keter-build:
	cabal install -j
	cp .cabal-sandbox/bin/positionsites positionsites
	tar czfv positionsites.keter positionsites config production.cfg static snaplets log/_blank

keter-deploy:
	scp positionsites.keter dbp@$(SERVER):/opt/keter/incoming

deploy: keter-build keter-deploy
