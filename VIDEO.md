---
title: Functional Reactive Programming with the WebAudio API
published: false
description: A short intro to the PureScript WAGs library
tags: 
//cover_image: https://direct_url_to_image.jpg
---

_This is the transcription of a video for [WAC 2021](https://webaudioconf2021.com/presenters-instructions/)._

The Web Audio API provides creators with the ability to build and share rich media applications that, until recently, were only possible in professional digital audio tools like SuperCollider or Apple Logic.

Let's first listen to this glitch example, available on the web at [purescript-wags-atari-speaks.surge.sh](https://purescript-wags-atari-speaks.surge.sh/).

> Example of atari speaks

The web can also do some heavy lifting with synthesis. For example, listen to this glistening pad at [mikesol.github.io/synth-lib](https://mikesol.github.io/synth-lib/).

> Example of synth lib

And finally, get your microphones ready and check out this link: [mikesol.github.io/feedback](https://mikesol.github.io/feedback/).

> Example of microphone

What all of these examples have in common is that they were all created using a technique called functional reactive programming, or FRP. In this video, I'm going to talk more about FRP as well as a related concept called a `Stream`. I'll show you how FRP and Streams provide a rich tool-set for working with Web Audio.

# FRP and Behaviors

From a consumer's perspective, the Web Audio API is nothing more than a graph. The graph has resources like oscillators, filters and gain nodes, and there are certain rules about what can be connected to what, but at the end of the day it's just a graph. Let's listen to the output of a WebAudio graph and look at the graph in the Google Chrome WebAudio inspector.

> Show inspector and play audio

To change this graph over time, we can _sample_ a representation of the graph, just like we sample audio.  We can then distill that sample into a set of instructions for the WebAudio API. If we sample the graph at a fast enough rate, ie 50 times a second, we can capture enough nuances to communicate a vivid soundscape to listeners' ears. Let's listen to the same WebAudio graph, but now it will update over time. We can see these updates in the WebAudio inspector.

> Show inspector and play audio

_Sampling_ is the bread-and-butter operation of functional reactive programming.

> Behavior in PurSuit.

In FRP, there's a data type called a `Behavior` that represents the state of some computational world. In our case, it will represent the state of the Web Audio graph. Behaviors are then sampled based on some external stimulus. This stimulus can be user-initiated, like a mouse click, or pre-defined, like a polling loop.

[example]

# Declarative vs imperative programming

Let's look at how this example was put together in PureScript using a library called [`purescript-wags`](https://github.com/mikesol/purescript-wags).

This example shows a crucial distinction between functional programming and imperative programming. In imperative programming, like JavaScript or TypeScript, we spend a lot of time telling the language how to connect objects and in what order. This has two pitfalls:

- It requires a lot of code to be productive.
- It is the source of many subtle errors, like calling a function with incorrect input, forgetting to call a function or calling functions in a wrong order. 

Functional programming, on the other hand, makes declarations about how the world should look.

> Show IDE.

For example, we used the type `MyScene` to declare that the audio graph should contain three oscillators and three gain nodes. We then used the function `change` to show how frequencies and volumes should vary as a function of time. We didn't need to manually connect, create, disconnect or destroy any audio nodes. Under the hood, libraries like `purescript-wags` and its dependencies make more fine-grained decisions, and it is only at the most low-level code that the actual imperative interactions with the WebAudio API happen.

So, in a relatively terse format, we're already able to build a rich Web Audio application. Moreover, there is no DSL - this is a vanilla PureScript program, which means the power of the whole language is at our disposal.

# Events

The other important concept in functional reactive programming is the `Event`. Unlike behaviors, events are _discreet_ functions of time. That means that, whereas a `Behavior` can be sampled at any time, `Events` are ephemeral and must be dealt with immediately. One common event is a mouse click. Let's see how we can capture a mouse down event in `purescript-wags`.

> Show Mouse Event Example

This technique can be extended to lots of different events, including MIDI events and live coding.

# Dynamic graphs

So far, we've been working with graphs whose entire structure is defined for the duration of the example. We haven't added additional nodes or rearranged their connections. This is fine for most web audio applications, but for very large graphs, it's often easier to provision and free nodes as the application evolves.

Even more often, the structure of an application's control-rate data will change. For example, at a certain moment we may need data to influence a bank of oscillators whereas at another moment in time, we may need data to influence a filter bank.

In both of these cases, the types used to construct and manipulate the web audio graph need to change.  At the same time, on the low level, nothing changes. At the end of the day, all the Web Audio API sees is a series of low-level commands, like `createOscillator()` or `setFrequency()`.  This is a common pattern in any programming language: there's an inner dynamism that is erased during a rendering phase, leaving only low-level instructions. In fact, _all_ programs can be thought of like this: the internal life of a program is much more nuanced than what is presented on a screen or output from a speaker. In functional programming, this inner dynamism is expressed using something called _existential types_.

An existential type is a type that exists on the inside of a larger computation. The outer part of the computation is aware of the type's existence, but no more than that: it doesn't know and doesn't need to know the inner type's definition because that type is erased during a rendering phase. At the same time, it has to know that _some type_ exists because it handles that type, usually by passing it from a producer of the type to a consumer.

Let's see how this works in `purescript-wags`. `Scene` is the type of the object containing low-level web audio instructions. It is an infinite stream that, as you peel back the layers, produces more low-level instructions to feed to the web audio API. Let's see an example of scene in the PureScript repl.

The construction of a scene, however, admits two extra type parameters - one of the WebAudio graph and one of the control-data passed to the graph. These are our existential types.  Let's look at that type signature for `makeScene`.

The types `graph` and `control` both get erased in `Scene`. Sometimes folks use the word "heterogeneous" for this - `Scene` is a heterogeneous stream because, under the hood, its `graph` and `control` types differ from frame to frame of a scene.

Let's listen to this example rendered to the web audio API.

[example]

We can literally hear the existential types: when the type of the graph and control changes, the sound world changes. However, the `Scene` that is rendered erases this dynamism, providing only the low-level instructions we saw in the REPL. 

For more examples of existential types in PureScript, you can check out a repo called [`purescript-exists`](https://pursuit.purescript.org/packages/purescript-exists/5.0.0/docs/Data.Exists#t:Exists). It gives an example of using existential types to construct a stream right in the README, and this example was very helpful for me as I built enough intuition about existential types to build `purescript-wags`. 

# Proof terms

Existential types do more than just facilitating dynamic streams. They also provide an interface to yield control from a creator to an engine and back again.

Looking again at the signature of `makeScene`, there is a `proof` type that can only be set for the initial frame. In the continuation function, there is a _different_ proof type that we cannot supply because all we know is that it exists. Let's try to provide a value to this function and you'll see what I mean.

In short, we can never drum up a `proofB` because we don't know what it is - all we know is that it exists. That means that the function provided as the second argument to `makeScene` can only _ever_ be called by an underlying engine that knows what `proofB` is. This flow gives the engine the opportunity to make all the necessary rendering calculations before it calls the continuation function, at which point it yields control back to the consumer.

Yielding control to and from an engine is a critical flow in any rendering application, and existential types provide an elegant way, via proof terms, to achieve this in a functional paradigm.

# Conclusion

In conclusion, functional reactive programming using existential types provides a rich, expressive, and type-safe framework for creating interactive web audio applications.  In this short presentation, we've seen how they can be used to create synthetic sounds, musical instruments and full-length works. Languages like PureScript, Haskell, Idris and Agda have proven to be great partners in the creative journey, and the ability to bring these concepts to the Web represents an unprecedented opportunity to share works in a way we've never done before. I very much hope you get a chance to experiment with these ideas, and I'm excited to hear where they lead you!