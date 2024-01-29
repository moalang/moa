run:
	kotlinc HM.kt && kotlin HMKt
watch:
	@make run
	@-fswatch -0 -o HM.kt | xargs -I {} -n1 -0 make run
