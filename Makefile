

all : dist/cl-nntp.tar.gz

dist/cl-nntp.tar.gz : *.lisp *.asd | dist
	tar cfz dist/cl-nntp.tar.gz -C .. cl-nntp --exclude=.git* --exclude=dist

dist :
	mkdir dist

.PHONY : clean
clean :
	rm -fr dist
