dev:
	ghcid --command "stack ghci --ghci-options -fdiagnostics-color=always" --test "DevMain.run"
.PHONY: dev
