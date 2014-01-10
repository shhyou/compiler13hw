float delta = 1.0;

int func(float num) {
    int temp;
    float dumb;
    if (num > 0) {
        temp = num + delta;
    } else {
        temp = num - delta;
    }
    return temp;
}


int main() {
    float num;

    write("Enter number "); 

    num = fread(); 

    write(func(num));
    write("\n");

    return 0;
}
