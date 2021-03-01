CFLAGS= -f 60


test :
	racket src/tests.rkt $(CFLAGS)
 

documentation :
	 scribble --dest doc src/actors_documentation.scrbl
	
	
	

play : 
	racket src/main.rkt

clean :
	rm -f *~
	rm -f compiled
	cd doc/ && rm -f *pdf && rm -f *html && rm -f *css &&  rm -f *js
