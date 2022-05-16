int main(){
    int a = 2;
    print a; //2

    {
        int a = 3;
        print a; //3
        a++;
        print a; //4
    }

    print a; //2 <- without change

    return 0;
}