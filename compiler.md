Context


Data<A>
  .query<B>(C)
  .map<B>(|x| x as i8)
  .query<i8>()
B: Derive<A, (C)>
