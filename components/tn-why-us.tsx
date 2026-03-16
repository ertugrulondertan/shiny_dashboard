"use client"

import { Check } from "lucide-react"

const reasons = [
  {
    title: "Creative Strategy Tailored to Your Brand",
    description: "Every brand is unique. We develop custom strategies that align with your goals and resonate with your audience.",
  },
  {
    title: "Professional Content Production",
    description: "High-quality visuals and content that elevate your brand above the noise and capture attention.",
  },
  {
    title: "Fast and Reliable Communication",
    description: "We value your time. Expect quick responses, clear updates, and seamless collaboration throughout.",
  },
  {
    title: "Modern Design That Builds Trust",
    description: "Contemporary aesthetics that position your brand as professional, credible, and forward-thinking.",
  },
  {
    title: "Results-Driven Digital Execution",
    description: "We focus on metrics that matter. Every campaign is designed to deliver measurable business outcomes.",
  },
]

export function TNWhyUs() {
  return (
    <section className="py-24 md:py-32 bg-card/30">
      <div className="max-w-6xl mx-auto px-6">
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-12 lg:gap-20 items-center">
          {/* Left Column - Text */}
          <div>
            <span className="inline-block px-4 py-1.5 rounded-full border border-border/50 bg-card/50 text-sm text-muted-foreground mb-4">
              Why TN Creative
            </span>
            <h2 className="text-3xl md:text-4xl lg:text-5xl font-bold text-foreground mb-6 text-balance">
              Your Success Is Our Mission
            </h2>
            <p className="text-lg text-muted-foreground text-pretty">
              We combine creativity with strategy to help your brand achieve real, measurable results. 
              Here's what sets us apart.
            </p>
          </div>

          {/* Right Column - Points */}
          <div className="space-y-6">
            {reasons.map((reason, index) => (
              <div key={index} className="flex gap-4">
                <div className="flex-shrink-0 w-8 h-8 rounded-full bg-primary/10 flex items-center justify-center">
                  <Check className="w-4 h-4 text-primary" />
                </div>
                <div>
                  <h3 className="text-lg font-semibold text-foreground mb-1">
                    {reason.title}
                  </h3>
                  <p className="text-muted-foreground text-sm leading-relaxed">
                    {reason.description}
                  </p>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </section>
  )
}
