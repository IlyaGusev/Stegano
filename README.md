#### Install ####
```
git clone https://github.com/IlyaGusev/stegano
cd stegano
wget https://erlang.mk/erlang.mk
make -f erlang.mk bootstrap-rel
```
  
#### Run ####
```
make run
```

#### Usage ####
```
curl -X PUT -F 'data=@path-to-file' localhost:8080/
curl -X GET 'localhost:8080/?text=<your text>&imagename=<image file name on server>' -o <cyphered filename>
curl -X DELETE -d 'filename' localhost:8080/
```