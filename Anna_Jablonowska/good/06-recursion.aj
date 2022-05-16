int recfun(int i){
    if(i >= 5){
        return i;
    } else{
        return recfun(i + 2);
    }
}

int main(){
    int a = recfun(2);
    print a;
    return 0;
}