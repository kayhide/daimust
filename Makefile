dev:
	ghcid --command "stack ghci --main-is DevMain" --test "DevMain.run"

.PHONY: dev
