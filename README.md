# Task 3 - multixo #
*(пока 6 баллов)*

#### Install ####
```
npm install
tsc
```

#### Run ####
```
nodejs server/index.js
```

# Task 4.2 - stegano #
*(10 баллов)*

#### Install ####
```
git clone https://github.com/IlyaGusev/proglang
cd stegano
wget https://erlang.mk/erlang.mk
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
