package com.github.xplosunn.statham

case class Channel[In, Out](label: String, in: JsonDescriptor[In], out: JsonDescriptor[Out])