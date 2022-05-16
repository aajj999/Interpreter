int funcVal(int a){
    a = a + 3;
    return a;
}

string funcInOut(in int ia, out int ob){
    ob = ia + 4;
    return "Result is not here";
}

int main(){
    /*argument passed by value
     -function return the result
     -variables don't change their value*/
    int a = 2;
    print funcVal(a); //passing value from a
    print a; //value stays the same - 2

    /*argument passed as in/out
     -function doesn't return the result
     -variables may change their value*/
    int b;
    funcInOut(a, b); //b passed as out argument
    print b; //value changed  - 6
    print a; //value stays the same - 2

    return 0;
}