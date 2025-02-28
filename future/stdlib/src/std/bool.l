data Bool = True | False

(&&) : Bool -> Bool -> Bool
True  && b = b
False && _ = False

(||) : Bool -> Bool -> Bool
_ || True  = True
b || False = b

not : Bool -> Bool
not True  = False
not False = True

otherwise : Bool
otherwise = True
