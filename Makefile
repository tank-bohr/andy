.PHONY: pods apps shell kb-app-show kb-app-shell remote-console build-and-push

NAMESPACE ?= default

pods:
	@kubectl get pods -n $(NAMESPACE)

apps:
	@kubectl get pod -n $(NAMESPACE) -l "app=andy" -o name

shell: kb-app-shell

remote-console: kb-app-remote_console

build-and-push: docker-build docker-push

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

kb-app-remote_console:
	@kubectl exec -it $(APP) -n $(NAMESPACE) -- ./bin/app remote_console

docker-build:
	@docker build . -t tankbohr/andy

docker-push:
	@docker push tankbohr/andy:latest
