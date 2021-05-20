.PHONY: test pods apps shell kb-app-show kb-app-shell remote-console build-and-push

NAMESPACE ?= default
PORT := 6379

test:
	rebar3 ct --sname=test

get-pods:
	@kubectl get pods -n $(NAMESPACE) -o wide

apps:
	@kubectl get pod -n $(NAMESPACE) -l "app=andy" -o name

apply:
	@kubectl apply -f k8s/andy.yml

shell: kb-app-shell

remote-console: kb-app-remote_console

build-and-push: docker-build docker-push

rebuild: kb-cleanse build-and-push apply get-pods

################################################################################

kb-app-%: APP=$(shell \
	kubectl get pods \
		-n $(NAMESPACE) \
		-o jsonpath='{.items[?(@.metadata.labels.app == "andy")].metadata.name}' \
	| awk '{print $$1;}')

kb-app-show:
	@echo $(APP)

kb-app-shell:
	@kubectl exec -it $(APP) -n $(NAMESPACE) -- sh

kb-app-logs:
	@kubectl logs -f $(APP)

kb-app-remote_console:
	@kubectl exec -it $(APP) -n $(NAMESPACE) -- ./bin/app remote

kb-app-port-forward:
	@kubectl port-forward $(APP) $(PORT)

kb-cleanse:
	@kubectl delete service andy-service
	@kubectl delete deployments andy-deployment

docker-build:
	@docker build . -t tankbohr/andy

docker-push:
	@docker push tankbohr/andy:latest

docker-run:
	@docker run --rm \
		-p 6379:6379 \
		-e "POD_IP=127.0.0.1" \
		-e "LOG_LEVEL=debug" \
		tankbohr/andy

docker-run-shell:
	@docker run --rm -it \
		-p 6379:6379 \
		-e "POD_IP=127.0.0.1" \
		-e "LOG_LEVEL=debug" \
		tankbohr/andy ./bin/app start_iex

open-zipkin:
	open http://localhost:9411/zipkin/
