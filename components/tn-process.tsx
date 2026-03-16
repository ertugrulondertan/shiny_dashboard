"use client"

const steps = [
  {
    number: "01",
    title: "Discovery",
    description: "We learn about your brand, goals, audience, and competition to understand what makes you unique.",
  },
  {
    number: "02",
    title: "Strategy",
    description: "We develop a tailored plan that aligns creative direction with your business objectives.",
  },
  {
    number: "03",
    title: "Production",
    description: "Our team brings the strategy to life through high-quality content and design execution.",
  },
  {
    number: "04",
    title: "Launch & Growth",
    description: "We deploy, monitor, and optimize to ensure continuous improvement and measurable results.",
  },
]

export function TNProcess() {
  return (
    <section className="py-24 md:py-32 bg-card/30">
      <div className="max-w-6xl mx-auto px-6">
        {/* Section Header */}
        <div className="text-center mb-16">
          <span className="inline-block px-4 py-1.5 rounded-full border border-border/50 bg-card/50 text-sm text-muted-foreground mb-4">
            How We Work
          </span>
          <h2 className="text-3xl md:text-4xl lg:text-5xl font-bold text-foreground mb-4 text-balance">
            Our Process
          </h2>
          <p className="text-lg text-muted-foreground max-w-2xl mx-auto text-pretty">
            A proven approach that delivers consistent, exceptional results for every project.
          </p>
        </div>

        {/* Process Steps */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          {steps.map((step, index) => (
            <div
              key={index}
              className="relative group"
            >
              {/* Connector Line (hidden on last item and mobile) */}
              {index < steps.length - 1 && (
                <div className="hidden lg:block absolute top-8 left-[calc(50%+40px)] right-0 h-px bg-border/50 -z-10" />
              )}

              <div className="p-6 rounded-2xl border border-border/50 bg-card/50 hover:bg-card/80 transition-all h-full">
                {/* Step Number */}
                <div className="w-16 h-16 rounded-2xl bg-primary/10 flex items-center justify-center mb-6 group-hover:bg-primary/20 transition-colors">
                  <span className="text-2xl font-bold text-primary">{step.number}</span>
                </div>

                {/* Content */}
                <h3 className="text-xl font-semibold text-foreground mb-3">
                  {step.title}
                </h3>
                <p className="text-muted-foreground text-sm leading-relaxed">
                  {step.description}
                </p>
              </div>
            </div>
          ))}
        </div>
      </div>
    </section>
  )
}
