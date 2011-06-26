// Example Featherweight Java file
class X extends Object {
    Y fst;
    Object snd;
    X(Y fst, Object snd) {
        super();
        this.fst = fst;
        this.snd = snd;
    }
    Y exampleMethod() {
        return new Y(this.fst, this.snd, new Object());
    }
}

class Y extends X {
    Object thrd;
    Y(Y fst, Object snd, Object thrd) {
        super(fst, snd);
        this.thrd = thrd;
    }
    Y exampleMethod() {
        return (Y) ((Y) new X((Y) new X(new Y((Y) new X((Y) new Object(), new Object()), new Object(), new Object()), new Object()).snd, new Object())).thrd;
    }
}

