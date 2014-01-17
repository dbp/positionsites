USER = host
SERVER = direct.positionsites.com

all:
	cabal install -fdevelopment && ./dist/build/positionsites/positionsites

run:
	./dist/build/positionsites/positionsites

deploy: send
	ssh $(USER)@$(SERVER) "/var/www/scripts/deploy.sh"

send:
	cabal install
	scp angel.conf $(USER)@$(SERVER):
	scp dist/build/positionsites/positionsites $(USER)@$(SERVER):positionsites-new
	rsync --checksum -avz -e ssh scripts/* $(USER)@$(SERVER):scripts
	rsync --checksum -avz -e ssh static/* $(USER)@$(SERVER):static
	rsync --checksum -avz -e ssh snaplets/* $(USER)@$(SERVER):snaplets

start:
	ssh $(USER)@$(SERVER) "/var/www/scripts/start.sh"

migrate:
	rsync --checksum -ave 'ssh '  migrations/* $(USER)@$(SERVER):migrations
	ssh $(USER)@$(SERVER) "/var/www/moo.sh upgrade"

rollback:
	ssh $(USER)@$(SERVER) "/var/www/scripts/rollback.sh"
