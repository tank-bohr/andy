.PHONY: test pods apps shell kb-app-show kb-app-shell remote-console build-and-push

NAMESPACE ?= default
PORT := 6379
Q := @

test:
	$(Q) rebar3 ct --sname=test

get-pods:
	$(Q) kubectl get pods -n $(NAMESPACE) -o wide

apps:
	$(Q) kubectl get pod -n $(NAMESPACE) -l "app=andy" -o name

apply:
	$(Q) kubectl apply -f k8s/andy.yml

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
	$(Q) echo $(APP)

kb-app-shell:
	$(Q) kubectl exec -it $(APP) -n $(NAMESPACE) -- sh

kb-app-logs:
	$(Q) kubectl logs -f $(APP)

kb-app-remote_console:
	$(Q) kubectl exec -it $(APP) -n $(NAMESPACE) -- ./bin/app remote

kb-app-port-forward:
	$(Q) kubectl port-forward $(APP) $(PORT)

kb-cleanse:
	$(Q) kubectl delete service andy-service
	$(Q) kubectl delete deployments andy-deployment

docker-build:
	$(Q) docker build . -t tankbohr/andy

docker-push:
	$(Q) docker push tankbohr/andy:latest

docker-run:
	$(Q) docker run --rm \
		-p 6379:6379 \
		-e "POD_IP=127.0.0.1" \
		-e "LOG_LEVEL=debug" \
		tankbohr/andy

docker-run-shell:
	$(Q) docker run --rm -it \
		-p 6379:6379 \
		-e "POD_IP=127.0.0.1" \
		-e "LOG_LEVEL=debug" \
		tankbohr/andy ./bin/app start_iex

open-zipkin:
	$(Q) open http://localhost:9411/zipkin/
