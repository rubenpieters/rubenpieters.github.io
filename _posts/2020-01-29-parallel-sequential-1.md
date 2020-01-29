---
title: Parallel/Sequential versus OnStart/OnComplete
layout: post
categories: [Programming]
---

In this post I give some thoughts on the use of `parallel` and `sequential` in favor of `onStart` and `onComplete` for an animation library. The reason why I think it is interesting to look at is because the latter seems to be much more common in many animation libraries such as for example [anime.js](https://animejs.com/documentation/#beginComplete), [GSAP](https://greensock.com/docs/v2/TweenMax/eventCallback()), [Velocity.js](https://github.com/julianshapiro/velocity/wiki/Option---Begin) or [Phaser Tweens](https://photonstorm.github.io/phaser3-docs/Phaser.Tweens.Tween.html). However, as I have mentioned [before]({% post_url 2019-07-09-animation-dsl-1 %}), I am not very thrilled by this interface for animations. However, I wasn't quite able to articulate my annoyance beyond a simple "I don't really like it". Since then, I think I have found a way to express what exactly is bothering me with this common approach to describe animations.

# Syntax & Semantics

In linguistics there is the concept of *syntax* and *semantics*. The syntax comprises of the possible constructions that can be made in a language, while semantics is the actual meaning ascribed to those constructions. This terminology is equally applicable to domain-specific languages (DSLs), such as for example for expressing animations.

I think the key lies in the difference of the expected *semantics* of the two approaches. So, as a starting point, I want to gave a basic explanation of these concepts.

## Syntax

First, let's define syntax for two different animation DSLs: a *combinator*-style DSL and a *callback*-style DSL.

### Combinator-Style DSL

In the combinator-style DSL we have three main concepts: basic animations, sequential animations and parallel animations. Basic animations are atomic units of animation. A simple example of such unit is the linear interpolation between a from and to value. A more complex example is the morphing of a shape into another shape. In contrast with the atomic units of animation are the composed animations. A sequential animation is the sequential composition of two animations, while the parallel animations is the parallel composition of two animations. We can describe the syntax for this DSL in [Backus-Naur form (BNF)](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form).

```
Combinator-Style BNF

<animation> ::= basic
              | <animation> parallel <animation>
              | <animation> sequential <animation>
```

If you are not familiar with BNF, it gives a convenient notation for describing syntax. The rules above can be read as follows: an animation is either a basic animation OR an animation in parallel with another animation OR an animation in sequence with another animation. The reference to animation is a recursive reference, so the mention of `<animation>` in the `parallel` or `sequential` declarations refers to the definition itself. Below are some examples that can be expressed with this syntax:

```
basic
basic parallel basic
basic sequential basic
(basic parallel basic) sequential basic
basic parallel (basic sequential basic)
(basic parallel basic) sequential (basic parallel basic)
...
```

The expression `basic` can be replaced with any suitable basic animation, such as `change box alpha from 0 to 1 for 1 second` or `morph box from square to circle for 2 seconds`. Which basic animations are or are not available isn't relevant for the points made in this post. Rather, the focus of this post is on the different styles of composing animations.

### Callback-Style DSL

In the callback-style DSL we similarly have three basic concepts: basic animations and composed animations using either `onStart` or `onComplete`. We can think of `x onStart y` as shorthand for something like `x.onStart(() => y)`, it registers animation `y` to play when animation `x` starts, and similarly for `onComplete` to register on the completion of an animation. Of course, for now this is merely syntax and the exact meaning will be decided once we define semantics.

```
Callback-Style BNF

<animation> ::= basic
              | <animation> onStart <animation>
              | <animation> onComplete <animation>
```

Again, for illustrative purposes, let's look at some examples that can be expressed with this syntax:

```
basic
basic onStart basic
basic onComplete basic
(basic onStart basic) onComplete basic
basic onStart (basic onComplete basic)
(basic onStart basic) onComplete (basic onStart basic)
...
```

## Semantics

The syntax we described does not mean anything yet until we give semantics for it. These semantics can be whatever we want. We could create an awkward notation for simple arithmetic by stating that the meaning of `basic` is `1`, the meaning of `parallel` is `+` and the meaning of `sequential` is `*`. But, we are in the process of creating a DSL for animations, so probably our semantics should have something to do with those.

There are different ways of giving meaning to syntax. Here, we give so-called [*denotational semantics*](https://en.wikipedia.org/wiki/Denotational_semantics). This means that the meaning of syntax is described by certain mathematical objects. For example, the mathematical object used as domain for the awkward syntax of arithmetic is the natural numbers.

For semantics of animations we work a bit more informally. The domain for this context is a *timeline*. This timeline describes what is going on in the animation for each point in time.

For example, the animation in this picture:

<center><img src="{{ '/assets/par-seq-1/color_anim.gif' | relative_url }}"></center>

can be described with the following timeline:

<center><img src="{{ '/assets/par-seq-1/anim_color_timeline.png' | relative_url }}"></center>

From `t=0` until `t=1` the background fades from black to blue. Then, at the same time, from `t=1` to `t=2` the left part fades from blue to red and the right part fades from blue to yellow.

### Basic

The semantics of the `basic` expression is a simple box in the timeline. From `t=0` until `t=x`, where `x` is the length of the animation, a certain part of the application is transformed.

For example, the animation `fade background from black to blue for 1 second` has the following semantics:

<center><img src="{{ '/assets/par-seq-1/basic_fade.png' | relative_url }}"></center>

### Combinator-Style DSL

Typically, denotational semantics is *compositional*: the meaning of a composed expression is built out of the meaning of its sub-expressions. This is also the case here, since the semantics of an `x parallel y` expression is given by the semantics of `x` occurring at the same time in the timeline as the semantics of `y`. Visually, we can describe it as follows:

<center><img src="{{ '/assets/par-seq-1/semantics_parallel.png' | relative_url }}"></center>

Similarly, for an `x sequential y` expression, where `y` occurs after `x` in the timeline:

<center><img src="{{ '/assets/par-seq-1/semantics_sequential.png' | relative_url }}"></center>

Remember that each of the `x` or `y` animations can in turn be composed animations. For example, the timeline of `(a parallel b) sequential c` is:

<center><img src="{{ '/assets/par-seq-1/combinator_example.png' | relative_url }}"></center>

### Callback-Style DSL (?)

We might expect that we can proceed in the same way for the semantics for the callback-style syntax. The `onStart` operator could have the same semantics as `parallel` and the `onComplete` operator could have the same semantics as `sequential`:

<center><img src="{{ '/assets/par-seq-1/semantics_callback_wrong.png' | relative_url }}"></center>

However, no actual implementation of the `onStart`/`onComplete` style adheres to these semantics. Take the following example: `(a onStart b) onComplete c`. If we copy the `parallel`/`sequential` semantics, then we get the timeline as we saw earlier:

<center><img src="{{ '/assets/par-seq-1/wrong_example_exp.png' | relative_url }}"></center>

The **actual** timeline resulting from the implementation, however, is the following:

<center><img src="{{ '/assets/par-seq-1/wrong_example_actual.png' | relative_url }}"></center>

Remember that `(a onStart b) onComplete c` in, for example, a JavaScript animation library is something like `a.onStart(() => b).onComplete(() => c)`. This means that both the `onStart` and `onComplete` callbacks are attached to the animation `a`. The animation `c` is attached to the end of animation `a`, it is not possible to attach a callback to the composed animation `a onStart b` as a whole[^1].

### Callback-Style DSL

Since the earlier semantics doesn't quite capture the actual behaviour, we have to come up with something else that does. The key difference is that composition of animations is not based on the start and end point of the composed sub-animations. Instead, we need to keep track of specific start and end points which signify where the second operand of an `onStart` and `onComplete` operator will be attached to.

For the callback-style dsl, we have to update the semantics of the `basic` expression to take into account these attachment points. So, the semantics of `basic` is again a box in the timeline, where the start attachment point is at the start of the animation and the end attachment point is at the end.

<center><img src="{{ '/assets/par-seq-1/basic_cb.png' | relative_url }}"></center>

Now, we can define the semantics of an `x onStart y` expression: attach animation y to the start attachment point of animation x. Similarly, the semantics for an `x onComplete y` expression: attach animation y to the end attachment point of animation x. Visually we can represent it like this:

<center><img src="{{ '/assets/par-seq-1/semantics_cb.png' | relative_url }}"></center>

The attachment points for the composed animation are derived from the attachment points of the first operand. 

Now, the actual implementation follows our defined semantics of `(a onStart b) onComplete c`:

<center><img src="{{ '/assets/par-seq-1/corrected_example.png' | relative_url }}"></center>

# Combinator-style > Callback-style

We have defined two different DSLs for animations, given some simple syntax and an informal explanation of the semantics. Is one better suited to the task of creating animations than the other? I prefer the combinator-style for two main reasons.

First, when I create an animation I think of it in terms of its behaviour as I have been describing them with timelines as in the pictures of this post. I find it a very convenient approach, which easily maps to getting the results you expect on the screen. Since the combinator-style operators more directly express the timeline semantics that I expect, it seems clearly better in this regard.

Second, the callback-style is less expressive when viewed as it is presented here[^2]. What I mean with that is: every timeline that is expressible in the callback-style is expressible with the combinator-style, but not vice versa. This can be derived from the fact that `(a parallel b) sequential c`, where the durations of `a` and `b` are not known upfront, can not be expressed with the callback-style and we can implement the `onStart`/`onComplete` operators using the `parallel`/`sequential` operators, see the [appendix](#appendix-a) for a sketch of the translation.

I am not the only one to come up with this idea of course. I could for example find some similar things in the [Qt animation library](https://doc.qt.io/qt-5/qtquick-statesanimations-animations.html) and [Ren'Py](https://www.renpy.org/doc/html/atl.html#parallel-statement). Even [GSAP](https://greensock.com/docs), which provides the callback-style, also provides a different feature for creating animations which they appropriately call [Timeline](https://greensock.com/get-started#sequencing-with-timelines). This feature is also based on the idea of describing timelines, similar to what `parallel`/`sequential` provide, and in the link they claim that 'choreographing complex sequences is crazy simple' with Timelines. What surprises me though, is that the approach isn't the most prevalent way animations are expressed.

# Conclusion

In conclusion, the main point I want to make is that I believe that the callback-style approach to describe animations does not seem like a good interface for animations. I argue this on the fact that it is not conducive to simple semantics of describing timelines, and by themselves the `onStart`/`onComplete` operators are less expressive than the `parallel`/`sequential` operators.

Have you ever used an animation library and felt something was off? Do you think of animations in terms of timelines? Or maybe you are a happy user of the callback-style and want to convince me of your ways? Feel free to discuss in the [accompanying reddit thread](TODO)! 

# Appendix A

In this appendix I give an implementation of the `onStart`/`onComplete` operators based in terms of the `parallel`/`sequential` operators.

The translation is based on the idea that given a sequence of `onStart` and `onComplete` callbacks on an animation, we can always transform it to the form `(a sequential (.. parallel ..)) parallel (.. parallel ..)`. For example, if we have `((((a onStart b) onComplete c) onStart d) onComplete e) onStart f` then the timeline described by that expression is the same timeline as `(a sequential (c parallel e)) parallel (b parallel d parallel f)`.

<center><img src="{{ '/assets/par-seq-1/appendix.png' | relative_url }}"></center>

In the translation, we ensure that everything is always of the form `(x1 sequential x2) parallel x3`, where `x1` is a basic animation. Then, we can describe the translation of each of the expressions in the callback-style DSL in function of the expressions in the combinator-style DSL. We do this by induction on the syntax definition.

<center><img src="{{ '/assets/par-seq-1/appendix2.png' | relative_url }}"></center>

The translation of a `basic` expression requires us to put it in the required form. We do this by using a simple `noop` expression, which is a basic animation that takes 0 seconds and does not do anything. The translation is then: `basic = (basic sequential noop) parallel noop`.

The translation of an `x onStart y` expression requires us to make use of the fact that `x` has been transformed to the form `(x1 sequential x2) parallel x3`, in other words: we apply the induction hypothesis on `x`. The translation should then add `y` onto the animations that are in parallel with `x1`. The translation is: `(x1 sequential x2) parallel (x3 parallel y)`.

Similarly, for `x onComplete y` we deconstruct `x` and put `y` in parallel with the animations occurring after `x`. The translation is: `(x1 sequential (x2 parallel y)) parallel x3`.

[^1]: Of course, to obtain a similar animation we can use `a onStart (b onComplete c)`. However, this is only possible if we know upfront that the animaton `b` is longer than animation `a`, and so this is not a general solution.
[^2]: In this section I am talking about the callback-style as defined here. In actual animation libraries, the syntax is not limited to the `onStart` and `onComplete` expression of course. For example, we usually also inherit `if then else` syntax from the host language.
