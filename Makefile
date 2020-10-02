LAMBDA_HANDLER := lambda-framework
ENVIRONMENT:= test
DOCKER_HUB := docker.io/akino1976
COMPOSE_DEFAULT_FLAGS := -f docker-compose.yaml

lambda_name:
	@echo $(LAMBDA_HANDLER)

build-lambda:
	docker build \
		-t $(LAMBDA_HANDLER)-$(ENVIRONMENT):latest \
		-t $(LAMBDA_HANDLER)-$(ENVIRONMENT)\
		-t $(DOCKER_HUB)/$(LAMBDA_HANDLER)-$(ENVIRONMENT):latest \
		-t $(DOCKER_HUB)/$(LAMBDA_HANDLER)-$(ENVIRONMENT) \
		src

build-%:
	docker-compose $(COMPOSE_DEFAULT_FLAGS) build $*

run-%:
	docker-compose $(COMPOSE_DEFAULT_FLAGS) run --rm  $*



stop-containers:
	docker-compose $(COMPOSE_DEFAULT_FLAGS) kill

clear-containers: stop-containers
	docker-compose $(COMPOSE_DEFAULT_FLAGS) rm --force

stop-all-containers:
	docker ps -q | xargs -I@ docker stop @

clear-all-containers: stop-all-containers
	docker ps -aq | xargs -I@ docker rm @

clear-volumes: clear-all-containers
	docker volume ls -q | xargs -I@ docker volume rm @

clear-images: clear-volumes
	docker images -q | uniq | xargs -I@ docker rmi -f @

clear-build-files:
	find . | grep "build[^/]*commit_[0-9a-f]\{7\}$$" | xargs rm -rf

clear-pytest-cache:
	sudo find . | grep -E "(\.pytest_cache)" | xargs rm -rf

clear-pycache:
	sudo find . | grep -E "(__pycache__|\.pyc|\.pyo$$)" | xargs rm -rf


run-target: build-lambda
	docker run --rm -it \
		-v ${PWD}/src:/var/task \
	$(LAMBDA_HANDLER)-$(ENVIRONMENT) \
	lambda_function.lambda_handler '{"name": "ruan"}'

run-postgre:
	docker-compose $(COMPOSE_DEFAULT_FLAGS) run pgadmin
