
// OPTIMAL 4x 4 point 4th order interpolation routine written by Olli Niemitalo.
// http://yehar.com/blog/wp-content/uploads/2009/08/deip-original.pdf
// (Original code mashed through faust. Faust reduced the number of adds/subtractions from 17 to 14, but weren't able to reduce the number of multiplications (14)).
static float cubic_interpolate(
                               float y0,float y1,
                               float y2,float y3,
                               float mu)
{

  if (true || ATOMIC_GET(root->editonoff))
    return cubic_interpolate_old(y0,y1,y2,y3,mu);
  
  // *: 14
  // +/-: 14
  float fTemp0 = y3; //(float)input3[i];
  float fTemp1 = y0; //(float)input0[i];
  float fTemp2 = (fTemp1 + fTemp0);
  float fTemp3 = y2; //(float)input2[i];
  float fTemp4 = y1; //(float)input1[i];
  float fTemp5 = (fTemp4 + fTemp3);
  float fTemp6 = (fTemp0 - fTemp1);
  float fTemp7 = (fTemp3 - fTemp4);
  
  float fTemp8 = mu - 0.5f; //((float)input4[i] - 0.5f);
  
  return
    (fTemp8
     *
     ((fTemp8
       *
       (((fTemp8
          *
          (((((0.00986988334359864f * fTemp5) - (0.00989340017126506f * fTemp2)) * fTemp8)
            +
            (0.15578800670302476f * fTemp6))
           -
           (0.46896069955075126f * fTemp7)))
         +
         (0.2519474493593906f * fTemp2))
        -
        (0.25194210134021744f * fTemp5)))
      +
      ((0.5374383075356016f * fTemp7) + (0.1542946255730746f * fTemp6))))
    +
    ((0.4656725512077849f * fTemp5) + (0.03432729708429672f * fTemp2));

}
