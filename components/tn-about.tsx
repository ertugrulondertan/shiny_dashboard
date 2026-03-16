"use client"

export function TNAbout() {
  return (
    <section id="about" className="py-24 md:py-32 bg-card/30">
      <div className="max-w-6xl mx-auto px-6">
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-12 lg:gap-20 items-center">
          {/* Left Column - Image/Visual */}
          <div className="relative">
            <div className="aspect-[4/3] rounded-2xl bg-muted/30 border border-border/50 overflow-hidden relative">
              {/* Decorative gradient */}
              <div className="absolute inset-0 bg-gradient-to-br from-primary/10 via-transparent to-primary/5" />
              <div className="absolute inset-0 flex items-center justify-center">
                <div className="text-center">
                  <span className="text-6xl font-bold text-primary/20">TN</span>
                  <p className="text-muted-foreground/50 mt-2 text-sm">Creative Studio</p>
                </div>
              </div>
            </div>
            
            {/* Floating accent */}
            <div className="absolute -bottom-4 -right-4 w-24 h-24 rounded-2xl bg-primary/10 border border-primary/20 -z-10" />
          </div>

          {/* Right Column - Content */}
          <div>
            <span className="inline-block px-4 py-1.5 rounded-full border border-border/50 bg-card/50 text-sm text-muted-foreground mb-4">
              About Us
            </span>
            <h2 className="text-3xl md:text-4xl lg:text-5xl font-bold text-foreground mb-6 text-balance">
              Creativity Meets Strategy
            </h2>
            <p className="text-lg text-muted-foreground mb-6 text-pretty">
              TN Creative is a creative agency focused on helping brands stand out with strong visuals, 
              smart strategy, and high-quality digital execution.
            </p>
            <p className="text-muted-foreground mb-8 text-pretty">
              Based in South Africa and serving clients globally, we combine local insight with 
              international standards to deliver work that truly makes a difference. Our team of 
              strategists, designers, and content creators work together to bring your vision to life.
            </p>

            {/* Stats Row */}
            <div className="grid grid-cols-3 gap-6 pt-6 border-t border-border/30">
              <div>
                <p className="text-3xl md:text-4xl font-bold text-primary">50+</p>
                <p className="text-sm text-muted-foreground mt-1">Projects</p>
              </div>
              <div>
                <p className="text-3xl md:text-4xl font-bold text-primary">30+</p>
                <p className="text-sm text-muted-foreground mt-1">Brands</p>
              </div>
              <div>
                <p className="text-3xl md:text-4xl font-bold text-primary">5+</p>
                <p className="text-sm text-muted-foreground mt-1">Years</p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>
  )
}
