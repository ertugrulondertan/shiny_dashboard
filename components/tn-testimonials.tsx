"use client"

import { Quote } from "lucide-react"

const testimonials = [
  {
    quote: "TN Creative transformed our entire brand presence. Their strategic approach and creative execution exceeded our expectations. The results speak for themselves - our engagement has tripled.",
    author: "Sarah Mitchell",
    role: "Marketing Director",
    company: "Pengo Foods",
  },
  {
    quote: "Working with TN Creative was a game-changer for our business. They understood our vision from day one and delivered content that truly resonates with our audience. Highly recommended.",
    author: "James Mokoena",
    role: "CEO",
    company: "Minetec Corporation",
  },
  {
    quote: "The team at TN Creative is exceptional. Professional, creative, and incredibly responsive. Our new website has already driven significant business growth.",
    author: "Lisa van der Berg",
    role: "Founder",
    company: "Urban Threads",
  },
]

export function TNTestimonials() {
  return (
    <section className="py-24 md:py-32">
      <div className="max-w-6xl mx-auto px-6">
        {/* Section Header */}
        <div className="text-center mb-16">
          <span className="inline-block px-4 py-1.5 rounded-full border border-border/50 bg-card/50 text-sm text-muted-foreground mb-4">
            Client Stories
          </span>
          <h2 className="text-3xl md:text-4xl lg:text-5xl font-bold text-foreground mb-4 text-balance">
            What Our Clients Say
          </h2>
          <p className="text-lg text-muted-foreground max-w-2xl mx-auto text-pretty">
            Don't just take our word for it. Here's what our partners have to say about working with us.
          </p>
        </div>

        {/* Testimonials Grid */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
          {testimonials.map((testimonial, index) => (
            <div
              key={index}
              className="relative p-6 md:p-8 rounded-2xl border border-border/50 bg-card/30 hover:bg-card/50 transition-all"
            >
              {/* Quote Icon */}
              <div className="w-10 h-10 rounded-full bg-primary/10 flex items-center justify-center mb-6">
                <Quote className="w-5 h-5 text-primary" />
              </div>

              {/* Quote Text */}
              <p className="text-foreground leading-relaxed mb-6 text-pretty">
                "{testimonial.quote}"
              </p>

              {/* Author */}
              <div className="flex items-center gap-3">
                <div className="w-10 h-10 rounded-full bg-muted/50 flex items-center justify-center">
                  <span className="text-sm font-medium text-muted-foreground">
                    {testimonial.author.split(' ').map(n => n[0]).join('')}
                  </span>
                </div>
                <div>
                  <p className="font-medium text-foreground text-sm">
                    {testimonial.author}
                  </p>
                  <p className="text-xs text-muted-foreground">
                    {testimonial.role}, {testimonial.company}
                  </p>
                </div>
              </div>
            </div>
          ))}
        </div>
      </div>
    </section>
  )
}
