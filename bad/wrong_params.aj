bool func(int a, bool b){
    if(b && a > 0){
        return true;
    }
    return false;
}

int main(){
    if(func(3, true, 7)){
        print "It can't be printed - func with wrong number of arguments";
    }
    return 3;
}